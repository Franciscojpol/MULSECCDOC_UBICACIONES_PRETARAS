      *****************************************************************
      *                                                                
      *
      * UBICACION DE BULTOS  P R E T A R A S
      *
      * 03/02/2015 -> Comprueba si la devolucion ha sido conformada y
      *               permite realizarla en el caso q no este conformada
      *
      * 05/04/2016 -> A Partir de este pgm ya no se realizan la comprobacion
      *               de las etiquetas de 22
      *                                                               
      *
      **
      ** 20/12/2017 -> Sustitucion de LNCTODASCE x EENTIDAD.
      *                                                               
      ** Cambio : #UDL  Autor: JESUSMSM  Fecha: 16/02/2022
      ** - Permitir ubicar devoluciones comprobando si ya está leída, es decir,
      **   si está en &DEVVAL.
      **   Con esta mejora se agiliza el proceso de ubicación, ya que no es
      **   necesario que la devolución esté conformada, sólo que esté leída.
      *                                                               
      ** Cambio : #UBC  Autor: JESUSMSM  Fecha: 12/12/2022
      ** - Permitir conformar las devoluciones que se están ubicando, en caso
      **   de que no estén conformadas.
      **   De esta forma, la ubicación y la conformación se realizarán en un
      **   solo paso.
      **
      ** Cambio: Lgmt-3555. Autor: FRANJPOL. Fecha: 27/03/2026
      ** [MULSECCDOC] UBLPRE Ubicacion de Bultos PRETARAS
      ** Adaptación a la multisección de las ubicaciones.
      ** Se realiza ajustes en la sentencia de SQLConforma.
      *****************************************************************

      *********************************************************
      * INDICADORES: CUALES Y PARA QUÉ
      *********************************************************
      * PARAMETROS :
      *
      *  &IA -->        Inicial Almacen
      *  &ID -->        Inicial C.D.
      *  &USER->        Usuario o Dni
      *  &PANTA->       Pantalla
      *  &COD  ->       Codigo Tipo Ubicacion '7' PreTaras
      *  &NOM  ->       Nombre Codigo Tipo Ubicacion
      *  &FIC  ->       Fichero 'LAMVTD14'
      *  &PGM  ->       Fichero 'KVUBLPRE'
      *  &LMA  ->       Longitud Maxima de la Etiqueta 24
      *  &LMI  ->       Longitud Minima de la Etiqueta 24
      *  &Cin-->        Marca Comercial
      *  &Na -->        Na
      *********************************************************
      *
      *********************************************************
      *
      *
      *********************************************************
      *
     H OPTION(*NODEBUGIO:*SRCSTMT)

      /COPY QCOPYSRC,STDDMPH

     FUBL002FM  CF   E             WORKSTN
      *
     FFSQLSTT   IF   E           K DISK
      *
     FAUBIBULT  UF A E           K DISK
      *
     FAUBIBUMA  IF   E           K DISK
      *
     FAUBIBOBS  IF   E           K DISK
      *
     FMAEUBT    IF   E           K DISK
      * Log Ubicaciones
     FLOGUBIBU  UF A E           K DISK
      *
     FAMVTDA    IF   E           K DISK
      *
     FEENTIDAD  IF   E           K DISK
     FADEVVAL   IF   E           K DISK                                         #UDL

     D*****************************************************************
      ******************************************************
     D FETC            S              5    DIM(10)
     D NOFETC          S              5    DIM(10)

     D DsFecha         DS
     D  DsDD                   1      2  0
     D  DsMM                   3      4  0
     D  DsAA                   5      8  0

      ** CASSIOPEA                                **********

     D                 DS
     D  WChkEtqx               1     24  0
     D  W00x                   1      2  0
     D  W22x                   3     24  0

     D                 DS
     D  WChkEtq                1     24  0
     D  W00                    1      2  0
     D  W22                    3     24  0

     D WCASIO          S             24A
     D  wTM            S              1A
     D  wTI            S              4A
     D  wTI24          S              6A
     D  wSE            S              1A
     D  wDO            S              7A
     D  wDD            S              2A
     D  wMM            S              2A
     D  wAA            S              2A
     D  wTP            S              1A
     D  wTE            S              1A
     D  wDC            S              1A

      ** CASSIOPEA NUMERICA
     D WCasiNumerica   S             24S 0

     D WEtqDgDesde     S              1S 0
     D WEtqDgHasta     S              1S 0
     D WEtqDesde       S             24S 0
     D WEtqHasta       S             24S 0

      **
     D ErrTienda       S              1N
     D SwPrimeraVez    S              1N
     D SwCampaNoact    S              1N
      ** Copia de la estructura de datos de estado de programa
      /copy qcopysrc,rpgpsdsi
      **
     D KNoDesc         C                   CONST('Comprobar Descripcion   ')
     D KUbiOcupa       C                   CONST('Ubicacion Ocupada')
     D KNumMax         C                   CONST('Sobrepasado Nº Bultos en Ubic-
     D                                     acion')
     D KYaUbica        C                   CONST('Bulto YA Ubicado')
     D KNuevaUbi       C                   CONST('Introducir NUEVA Ubicacion')
     D KNoEtq          C                   CONST('Etiqueta Incorrecta')
     D KMalFoA         C                   CONST('Mal Fila/Columna   ')
     D KMalPos         C                   CONST('Mal Posicion       ')
     D KMalCasio2      C                   CONST('La Casiopea no es de PreTara -
     D                                        ')
     D KMalCasio3      C                   CONST('La Casiopea no es de una TARA-
     D                                        ')
     D KMalMovim       C                   CONST('No existe el Movimento Tienda-
     D                                        ')
     D KPosSuper       C                   CONST('Existe posicion mayor en Ubic-
     D                                     acion')
     D KMalSecc        C                   CONST('Seccion Mal')
     D KMalTien        C                   CONST('Seleccion Tienda Mal')
     D KMalPais        C                   CONST('Seleccion Pais Mal')
     D KNoLeida        C                   CONST('Devolucion No Leida')         #UDL


      ** PARAMETROS*****************************************

     D DSUblEtU        DS
     D   DsCod                 1      5
     D   DsFoa                 6      8
     D   DsPos                 9     11

      *  VARIABLES  ****************************************
     D                 DS
     D  TIMSTM                 1     14  0
     D  HORSYS                 1      6  0
     D  FECSYS                 7     14  0
     D  DIASYS                 7      8  0
     D  MESSYS                 9     10  0
     D  ANOSYS                11     14  0

      *****************************************************************
      ** Copia de la estructura de datos de estado de programa

      /Copy Qcopysrc,RPG4_CAS22
      /Copy Qcopysrc,RPG4_CAS24

     ** Copia de la estructura de datos de Llamada a Observaciones
      /copy qcopysrc,RPG4_UBLOB
     ***
     ** Prototipos  para llamadas ---------------------------------
     ***
     *** Llamada a UBLOBS (Grabacion de Observaciones)
     ***
     D  Prg_UBLOBS     Pr                  ExtPgm('UBLOBS')
      *                                    PrmIA
     D                                2A
      *                                    PrmID
     D                                2A
      *                                    PrmCIN
     D                                1A
      *                                    PrmNA
     D                                7A
      *                                    PrmUser
     D                               10A
      *                                    PrmPanta
     D                               10A
      *                                    PrmTipUb
     D                                1A
      *                                    PrmNomUb
     D                               20A
      *                                    PrmCodUb
     D                                5A
      *                                    PrmFoaUb
     D                                3A
      *                                    PrmPosUb
     D                                3A
      *                                    PrmEtqUb
     D                               24A
      *                                    PrmPgmO
     D                               10A

       //#UDL
       // Recupera parametrización para saber si se ubica etiqueta ya leída
       Dcl-Pr Pgm_INZ018 ExtPgm('INZ018');
         *N Char(10);
         *N Char(10);
         *N Char(10);
         *N Char(10);
         *N Char(10);
         *N Char(10);
         *N Char(10);
         *N Char(10);
         *N Char(10);
         *N Char(10);
         *N Char(10);
         *N Char(10);
         *N Char(10);
         *N Char(10);
         *N Char(10);
         *N Char(10);
         *N Char(10);
         *N Char(10);
         *N Char(10);
         *N Char(10);
         *N Char(10);
       End-Pr;
       Dcl-S Par_Clave    Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Ent1     Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Ent2     Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Ent3     Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Ent4     Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Ent5     Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Ent6     Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Ent7     Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Ent8     Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Ent9     Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Ent10    Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Sal1     Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Sal2     Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Sal3     Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Sal4     Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Sal5     Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Sal6     Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Sal7     Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Sal8     Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Sal9     Char (  10 ) Inz(*Blanks) ;
       Dcl-S Par_Sal10    Char (  10 ) Inz(*Blanks) ;

       Dcl-S SoN          Char (   1 ) Inz('N');
       Dcl-S LeidaSN      Char (   1 ) Inz('N');
       Dcl-S SwEtqDevTie  Ind          Inz(*Off);
       Dcl-S WCuantos     Zoned(9:0)   Inz(*Zeros);
      *
       // [MULSECCDOC].............................
       // MultiSección - MultiDocumento..Lgmt-3555.
       Dcl-s WAnoc     Zoned(4:0)   Inz(*Zeros);
       Dcl-Ds DsHost;
          SqlTipP      Zoned(1:0);
          SqlTien      Zoned(9:0);
          SqlSecc      Zoned(1:0);
          SqlTipM      Char (1  );
          SqlDocu      Zoned(7:0);
          SqlAnoD      Zoned(4:0);
          SqlMesD      Zoned(2:0);
          SqlDiaD      Zoned(2:0);
          SqlTSal      Char (1  );
       END-DS;
       // Procedimientos...........................
       // Evalúa etiqueta EsMultisección
       Dcl-pr EsMultiseccion ind;        //Retorna  Indicador.
         csdAlmacen  like(IA) const;        //Recibe  Almacén.
         csdEtiqueta like(@Etiqueta) const; //Recibe Etiqueta.
       end-pr;
       // Verifica si se ha leído la etq y está en DEVVAL.
       dcl-pr EsLeidaDevval ind;      //Retorna Indicador.
         pEtiqueta like(XEtqu) const; //Recibe Etiqueta.
       end-pr;
      *
       // Obtiene Secciones - Documentos - Cantidades de la Etq.
       Dcl-Pr VerSeccionDocus ExtPgm('MVTSDO');
         *N Char( 2) const;
         *N Char(24) const;
         *N Char( 1);
         *N LikeDs(DsSecDocu) Dim(15);
       End-Pr;
      *
       Dcl-S  @Almacen   Char( 2)    Inz(*Blanks);
       Dcl-S  @Etiqueta  Char(24)    Inz(*Blanks);
       Dcl-S  @SeccDocu  Char( 1)    Inz(*Blanks);
       // Ds ----------------------------------
       Dcl-Ds @PDsSalida Dim(15)     Qualified;
          PDsSeccion     Char (1);
          PDsDocumento   Char (7);
          PCantidad      Char (5);
       End-Ds;
       Dcl-Ds DsSecDocu  Dim(15) Qualified;
          Seccion        Char ( 1);
          Documento      Char ( 7);
          Cantidad       Char ( 5);
       END-DS;
      *
       Dcl-C MultiSeccion                    'S';
       Dcl-C MultiDocumento                  'M';
       Dcl-C SQL_Encontrado              '00000';
       Dcl-C SQL_NoEncontrado            '02000';
       Dcl-S EtiquetaMultiseccion  Ind Inz(*Off);
      *****************************************************************

     C     *ENTRY        PLIST
     C                   PARM                    Ia                2
     C                   PARM                    Id                2
     C                   PARM                    LkUser           10
     C                   PARM                    LkPanta          10
     C                   PARM                    LkCod             1
     C                   PARM                    LkNom            20
     C                   PARM                    LkFic            10
     C                   PARM                    LkPgm            10
     C                   PARM                    LkLmA             3
     C                   PARM                    LkLmI             3
     C                   PARM                    Cin               1
     C                   PARM                    Na                7
      ******************************************************
     C                   EXSR      RTVUSU
     C                   MOVEL     RTUSU         USER
      *
     C                   Exsr      EstadoSql
      *
      * PROCESO DE LA RUTINA INICIAL
     C                   EXSR      INICIAL
      *
     C                   EXSR      PROCES
      *
      *    FIN DEL PROGRAMA
      *
      *
     C                   SETON                                        LR
      **
      ******************************************************
      *   SUBRUTINA INCIAL                                 *
      ******************************************************
     C     INICIAL       BEGSR

     C                   Move      LkLmA         WLonMax           3 0
     C                   Move      LkLmI         WLonMin           3 0

     C                   MoveL     LkNom         PTNOM

     C                   Exsr      IniPan0R

       // #UDL
       // Para REFPGM
          If 8 = 9;
             Open ADEVVAL;
          EndIf;

       // Comprueba si el Cd puede ubicar etiquetas que estén leídas
       // aunque no estén conformadas.
         ExSr LLama_KVINZ;
       // #UDL

     C                   ENDSR
      ******************************************************
      *   PROCESO DE PANTALLA                              *
      ******************************************************
     C     PROCES        BEGSR

     C                   SetOff                                       30

     C                   Exsr      ProcesoPan0R

     C                   EndSr
      ******************************************************
      *   PROCESO DE PAN 0
      ******************************************************
     C     ProcesoPan0R  BEGSR

     C                   SetOff                                       30

     C                   DoW       *In03 = *Off And
     C                             *In12 = *Off

     C                   Write     CABECE
     C                   Exfmt     Pan0R
      **
     C                   If        *In03
     C                   Leave
     C                   EndIf
     C                   If        *In12
     C                   Iter
     C                   EndIf

      * Activa siguiente ubicacion *In77 = *On
     C                   If        PSIGUB = 'S'
     C                   SetOn                                        77
     C                   Else
     C                   SetOff                                       77
     C                   EndIf

     C                   Exsr      IniPanFi
      * Si se solicita ubicar estableciendo FILTROS
     C                   If        PFILTR = 'S'
     C                   Exsr      ProcesoPanFi
     C                   Else
     C                   Exsr      IniPan1
     C                   Exsr      ProcesoPan1
     C                   EndIf
      *
     C                   EndDo

     C                   If        *In12 = *On
     C                   SetOff                                       12
     C                   EndIf

     C                   EndSr
      ******************************************************
      *   Proceso    PANFI Pantalla de Filtros
      ******************************************************
     C     ProcesoPanFi  BEGSR

     C                   DoW       *In03 = *Off And
     C                             *In12 = *Off
     C                   Write     CABECE
     C                   Exfmt     PANFI
     C                   Eval      PERROR = *Blanks

     C                   If        *In03
     C                   Leave
     C                   EndIf
     C                   If        *In12
     C                   Iter
     C                   EndIf
     C                   If        *In04
     C                   Exsr      Consultas
     C                   Iter
     C                   EndIf

     C                   Exsr      ValPanFi

     C                   If        *In30 = *Off
     C                   Exsr      IniPan1
     C                   Exsr      ProcesoPan1
     C                   EndIf

     C                   EndDo

     C                   If        *In12 = *On
     C                   SetOff                                       12
     C                   EndIf

     C                   EndSr
      ******************************************************
      *   Proceso    PAN1R
      ******************************************************
     C     ProcesoPan1   BEGSR

     C                   DoW       *In03 = *Off And
     C                             *In12 = *Off
     C                   Write     CABECE
     C                   Exfmt     PAN1R
     C                   Eval      PERROR = *Blanks

     C                   If        *In03
     C                   Leave
     C                   EndIf

     C                   If        *In12
     C                   Iter
     C                   EndIf

     C                   Exsr      ValPan1R

     C                   If        *In30 = *Off
     C                   Exsr      IniPan2
     C                   Exsr      ProcesoPan2
     C                   EndIf

     C                   EndDo

     C                   If        *In12 = *On
     C                   SetOff                                       12
     C                   EndIf

     C                   EndSr
      ******************************************************
      *   Proceso    PAN2
      ******************************************************
     C     ProcesoPan2   BEGSR

      *  Si *In77 = *On  Proponer siguiente Ubicacion
     C                   If        *In77 = *On
     C                   Exsr      Pan2SigUbi
     C                   Else
     C                   Exsr      Pan2Normal
     C                   EndIf

     C                   EndSr
      ******************************************************
      *   Proceso    PAN2  Siguiente Ubicacion
      ******************************************************
     C     Pan2SigUbi    BEGSR

     C                   DoW       *In03 = *Off And
     C                             *In12 = *Off
     C                   Write     CABECE
     C                   Z-Add     *Zeros        PUBLETQ
     C                   Exfmt     PAN2R
     C                   MoveL     *Blanks       PERROR

     C                   If        *In03
     C                   Leave
     C                   EndIf

     C                   If        *In12
     C                   Iter
     C                   EndIf

     C                   If        *In08
     C                   Exsr      Observaciones
     C                   Iter
     C                   EndIf

     C                   Exsr      ValPan2R

     C                   If        *In30 = *Off
     C                   Exsr      MoverAUBIBUL
     C                   Exsr      GrabarAUBIBULT
     C                   Exsr      ProponUbica
     C                   If        WPos = 999
     C                   SetOn                                        12
     C                   MoveL     KNuevaUbi     PERROR
     C                   EndIf
     C                   EndIf
     C                   EndDo

     C                   If        *In12 = *On
     C                   SetOff                                       12
     C                   EndIf

     C                   EndSr
      ******************************************************
      *   Proceso    PAN2  Normal
      ******************************************************
     C     Pan2Normal    BEGSR

     C                   DoW       *In03 = *Off And
     C                             *In12 = *Off
     C                   Write     CABECE
     C                   Z-Add     *Zeros        PUBLETQ
     C                   Exfmt     PAN2R
     C                   MoveL     *Blanks       PERROR

     C                   If        *In03
     C                   Leave
     C                   EndIf

     C                   If        *In12
     C                   Iter
     C                   EndIf

     C                   If        *In08
     C                   Exsr      Observaciones
     C                   Iter
     C                   EndIf

     C                   Exsr      ValPan2R

     C                   If        *In30 = *Off
     C                   Exsr      MoverAUBIBUL
     C                   Exsr      GrabarAUBIBULT
     C                   Move      UBLTIP        KTip
     C                   Exsr      CompruMaximo
     C                   Exsr      CuentaBultos
     C                   Add       1             NBUL
     C                   If        NBUL > WNumMax
     C                   MoveL     KNumMax       PERROR
     C                   EndIf
     C                   EndIf
     C                   EndDo

     C                   If        *In12 = *On
     C                   SetOff                                       12
     C                   EndIf

     C                   EndSr
      ******************************************************
      *   Valida     PAN1R
      ******************************************************
     C     ValPan1R      BEGSR

     C                   Setoff                                       30

      * Comprobacion de si existen ubicaciones con posicion superior a la
      * 1ª propuesta cuands se elige proponer ubicacion = 'S'

     C                   If        *In77 = *On
     C                   Exsr      MiraPosicion
     C                   EndIf
      *

     C                   Exsr      ValUBLETU

     C                   EndSr
      ******************************************************
      *   Valida     PAN2R
      ******************************************************
     C     ValPan2R      BEGSR

     C                   SetOff                                       30
     C                   If        PUBLETQ = *Zeros
     C                   SetOn                                        30
     C                   EndIf

     C                   Exsr      Mira5o16o24

     C                   If        *In30 = *Off
     C                   Exsr      ValEtiqueta
     C                   EndIf

      * Si existen Filtros se valida q la etiqueta duchada cumpla
     C                   If        *In30 = *Off
     C                   If        PFILTR = 'S'
     C                   Exsr      FiltrosOk
     C                   EndIf
     C                   EndIf

      * Comprobacion de si existen ubicaciones con posicion superior a la
      * 1ª propuesta cuands se elige proponer ubicacion = 'S'

     C                   If        *In77 = *On
     C                   Exsr      MiraPosicion
     C                   EndIf
      *

     C                   EndSr
      ******************************************************
      *   Valida si el bulto ya esta ubicado
      ******************************************************
     C     ValEtiqueta   BEGSR

     C                   Z-Add     0             WCuanto           9 0
     C                   Z-Add     0             WEtqDgDesde
     C                   Z-Add     9             WEtqDgHasta
     C                   Z-Add     WETQU         WEtqDesde
     C                   Z-Add     WETQU         WEtqHasta
     C                   Move      WEtqDgDesde   WEtqDesde
     C                   Move      WEtqDgHasta   WEtqHasta

     C/EXEC SQL
     C+     Select  count(*)
     C+                      into :WCuanto
     C+                      from AUBIBULT  where UBLETQ >= :WEtqDesde And
     C+                                           UBLETQ <= :WEtqHasta
     C/END-EXEC

     C                   If        WCuanto > 0
     C/EXEC SQL
     C+     Select  UBLCOD, UBLFOA, UBLPOS Into :WCOD, :WFOA, :WPOS
     C+                      from AUBIBULT  where UBLETQ >= :WEtqDesde And
     C+                                           UBLETQ <= :WEtqHasta
     C/END-EXEC
     C                   SetOn                                        30
     C****************** MoveL     KYaUbica      PERROR
     C                   Exfmt     WINUBI
     C                   EndIf

     C                   EndSr
      ******************************************************
      *   Mira si la Etiqueta es de Cassiopea   22S 0
      ******************************************************
     C     Mira5o16o24   BEGSR

     C                   Z-Add     PUBLETQ       WETQU            24 0

     C                   SetOn                                        25

     C                   Exsr      PreparaEtq
     C                   Exsr      ValCasio

     C                   EndSr
      ******************************************************
      *   Valida/Desglosa   Etiqueta de Ubicacion
      ******************************************************
     C     ValUBLETU     BEGSR

     C     KAubiBULT     Klist
     C                   Kfld                    KCod              5
     C                   Kfld                    KFoA              3 0
     C                   Kfld                    KPos              3 0

     C                   MoveL     PUBLCOD       DsCod
     C                   Move      PUBLFOA       DsFoA
     C                   Move      PUBLPOS       DsPos

     C                   MoveL     DsCod         KCod
     C                   Move      DsFoA         KFoA
     C                   Move      DsPos         KPos

     C                   Exsr      LeeChAUBIBULT
     C                   Exsr      CompruAUBIBULT

     C                   EndSr
      *********************************************************************
      * Comprobacion de si existen ubicaciones con posicion superior a lao
      * 1ª propuesta cuands se elige proponer ubicacion = 'S'
      *********************************************************************
     C     MiraPosicion  BEGSR

     C                   Z-Add     0             WCuanto           9 0

     C/EXEC SQL
     C+     Select  count(*)
     C+       into :WCuanto
     C+       from AUBIBULT  where UBLCOD = :PUBLCOD And
     C+                            UBLFOA = :PUBLFOA And
     C+                            UBLPOS > :PUBLPOS
     C/END-EXEC

     C                   If        WCuanto > 0
     C                   MoveL     KPosSuper     PERRO1
     C                   Else
     C                   Move      *Blanks       PERRO1
     C                   EndIf

     C                   EndSr
      ******************************************************
      *   Cuenta los bultos q hay en una Ubicacion
      ******************************************************
     C     CuentaBultos  BEGSR

     C                   Z-Add     0             WNBultos          3 0

     C/EXEC SQL
     C+     Select  count(*)
     C+                      into :WNBultos
     C+                      from AUBIBULT  where UBLCOD = :PUBLCOD And
     C+                                           UBLFOA = :PUBLFOA And
     C+                                           UBLPOS = :PUBLPOS
     C/END-EXEC

     C                   EndSr
      *********************************************************************
      *
      *********************************************************************
     C     ValCasio      BEGSR

     C                   Move      *Blanks       WTipmX            1
     C                   Z-Add     0             WTippX            1 0
     C                   Z-Add     0             WSeccX            1 0
     C                   Z-Add     0             WTienX            9 0
     C                   Z-Add     0             WDocuX            7 0
     C                   Z-Add     0             WDiaDX            2 0
     C                   Z-Add     0             WMesDX            2 0
     C                   Z-Add     2000          WAnoDX            4 0
     C                   Move      *Blanks       WTesaX            1

      **** PreTaras
     C                   If        LkCod = '7'
     C                   If        Es24@A = 'S'
     C                   If        Tm24@N <>  7
     C                   SetOn                                        30
     C                   MoveL     KMalCasio2    PERROR
     C                   Else
     C                   Exsr      Mover24Val
     C                   EndIf
     C                   EndIf
     C                   Else
     C                   SetOn                                        30
     C                   MoveL     KMalCasio2    PERROR
     C                   EndIf
      //Lgmt-3555......................................[MULSECCDOC]
        //Procedimiento verifica si la etiqueta de 24 es multisección
        If Es24@A = 'S';
           EtiquetaMultiseccion = EsMultiseccion(IA : XEtqu);
        ENDIF;
        //...........................................................
     C                   If        *In30 = *Off
     C                   Exsr      SqlConforma

       // #UDL
       // Comprueba si la etiqueta ya fue leída aunque no esté conformada,
       // en cuyo caso permitirá ubicarla igualmente.
          LeidaSN = 'N';
          If SoN = 'S';           // Cd parametrizado
           //Lgmt-3555: If *In20 = '1' Or SQLSTT = '21000';
           If *In20 = '1';        // Etiqueta existente
            SwEtqDevTie = *On;    // Es Devolución Tienda
            If WAnoC = 0;         // Devolución NO CONFORMADA
             Exsr MiraSiLeida;    // Si está leída permitimos ubicar
             If LeidaSN = 'N';    // Si no está leída preguntamos si leer
              PLEER = 'N';
              ExFmt WINLEER;
              If PLEER = 'S';
               Exsr Conforma;     // MVT284
              EndIf;
              Exsr MiraSiLeida;   // Comprobamos de nuevo si la etiqueta ha sido
              If LeidaSN = 'N';   // leída y no han salido con F3 del MVT284
               *In30 = *On;
                Eval PERROR = KNoLeida;
              EndIf;
             EndIf;
             LeaveSr;
            EndIf;
           Else;
            *In30 = *On;
             Eval PERROR = KNoLeida; // Etiqueta inexistente
             LeaveSr;
           EndIf;
          EndIf;
       // #UDL

          If SoN = 'N';           // Cd No parametrizado                        #UBC
      * 03/03/2015  ademas de buscar en la tabla FETC el valor devuelto
      * Preguntamos por SQLSTT = 21000 por si devuelve mas de una linea
      *
      * 07/04/2026: Se comenta la linea del SQLSTT='21000'. Lgmt-3555
      * Ya que el SQL lo he cambiado para evitar este error.
      *
     C**** *In20         IfEq      '1'              #Lgmt-3555
     C**** SQLSTT        OrEq      '21000'          #Lgmt-3555
     C     *In20         IfEq      '1'
     C                   If        WAnoC = 0
     C                   Eval      PCONF = 'N'
     C                   Exfmt     WINCONF
     C                   If        PCONF = 'S'
     C                   Exsr      Conforma
      * Comprueba de nuevo q la Cassio ha sido conformada y no han
      * salido con F12 del MVT284
     C                   Exsr      SqlConforma
     C**** *In20         IfEq      '1'              #Lgmt-3555
     C**** SQLSTT        OrEq      '21000'          #Lgmt-3555
     C     *In20         IfEq      '1'
     C                   If        WAnoC = 0
     C                   SetOn                                        30
     C                   MoveL     KMalMovim     PERROR
     C                   EndIf
     C                   EndIf
     C                   EndIf
     C                   EndIf
     C                   Else
     C                   SetOn                                        30
     C                   MoveL     KMalMovim     PERROR
     C                   EndIf
          EndIf;                                                                #UBC

     C                   EndIf

     C                   EndSr
      ******************************************************
      *   Comprueba si la casio esta conformada
      *
      * [MULSECCDOC] Adaptación a la Multisección.#Lgmt-3555
      * En el SQL ya busca por etiqueta por lo que conformará todos
      * los documentos. Aprovecho para realizar más ajustes:
      * Chequeo si algún doc está sin conformar, antes salía ERROR
      * SQL= -811/ SQLSTT= -21000 ahora con Fetch First se evita y
      * NO recupera más de una fila.
      ******************************************************
     C     SqlConforma   BEGSR
      *
        Exsr LimpiaValores;
      *
        Exec Sql
        Select MVANOC,
               MVTIPP, MVTIEO, MVSECO, MVTIPM, MVDOCU,
               MVANO,  MVMES,  MVDIA,  MVTSAL
          Into :WAnoc,              -- Valor devuelto principal para control
               :DsHost              -- Resto de valores del Sql definidos en DS
             From AMVTDA
            Where mvli34 = :Et24@A  -- Etiqueta PreTara
            Order By mvanoc Asc     -- Control NO CONFORMADOS primero
            Fetch First 1 Row Only;
     C*/OLD------------------------
     C*/EXEC SQL
     C*+     Select  MVANOC, MVTIPP, MVTIEO, MVSECO, MVTIPM, MVDOCU,             #UDL
     C*+             MVANO,  MVMES,  MVDIA,  MVTSAL                              #UDL
     C*+        Into :WAnoc,   :SqlTipP, :SqlTien, :SqlSecc, :SqlTipM,           #UDL
     C*+             :SqlDocu, :SqlAnoD, :SqlMesD, :SqlDiaD, :SqlTSal            #UDL
     C*+                From AMVTDA                                              #UDL
     C*+                Where MVLI34 = :Et24@A;
     C*/END-EXEC
     C*/-----------------------------------------------------------------------------
     C     SQLSTT        LOOKUP    FETC                                   20

     C                   EndSr
      ******************************************************
      *   Limpiar Variable de Control y valores SQL
      ******************************************************
          BegSr LimpiaValores;
            Reset WAnoc ;
            Clear DsHost;

          ENDSR;

      ******************************************************
      *   Llama al programa de Devoluciones MVT280 para conformar la Devo.
      ******************************************************
     C     Conforma      BEGSR

     C                   Call      'KVVT280UB'
     C                   Parm                    IA
     C                   Parm                    ID
     C                   Parm                    CIN
     C                   Parm                    NA
     C                   Parm      'R'           Radio             1
     C                   Parm      'N'           PideId            1
     C                   Parm      *Blanks       Libre1            1
     C                   Parm      *Blanks       Libre2            6
     C                   Parm      LkUser        ParmDni          10

     C                   EndSr
      ******************************************************
      *   Mueve para si la casiopea es de 24 Validar
      ******************************************************
     C     Mover24Val    BEGSR

     C                   Z-Add     Tp24@N        WTippX
     C                   Z-Add     Se24@N        WSeccX
     C                   Z-Add     Ti24@N        WTienX
     C                   Z-Add     Do24@N        WDocuX
     C                   Z-Add     Di24@N        WDiaDX
     C                   Z-Add     Me24@N        WMesDX
     C                   Add       An24@N        WAnoDX
     C                   Move      TE24@A        WTesaX                         Lgmt-3555
       // #UDL
          If Tm24@N = 7;
           WTipmX = 'T';
          EndIf;
       // #UDL

     C                   EndSr
      ******************************************************
      *   Comprueba si ubicacion ocupada AUBIBULT
      ******************************************************
     C     CompruAUBIBULTBEGSR

     C                   If        %Found(AUBIBULT)
     C                   Z-Add     0             WNBultos
     C                   Move      UBLTIP        KTip
     C                   Exsr      CompruMaximo
     C                   Exsr      CuentaBultos
     C                   Else
     C                   Setoff                                       30
     C                   Move      *Blanks       PERROR
     C                   EndIf

     C                   EndSr
      *******************************************************************
      *   Comprueba el Nº Maximo de bultos q se permiten en una ubicacion
      *   segun el tipo de bulto UBLTIP
      ********************************************************************
     C     CompruMaximo  BEGSR

     C     KAubiBUMA     Klist
     C                   Kfld                    KTip              1


     C                   Exsr      LeeChAUBIBUMA
     C                   If        Not %Found(AUBIBUMA)
     C                   Seton                                        30
     C                   MoveL     KUbiOcupa     PERROR
     C                   Else
     C                   MoveL     *Blanks       PERROR
     C                   Z-Add     MAXNUM        WNumMax           3 0
     C                   EndIf

     C                   EndSr
      ******************************************************
      *   Mueve para grabar la ubicacion
      ******************************************************
     C     MoverAUBIBUL  BEGSR

     C                   Move      KCod          UBLCOD
     C                   Z-Add     KFoA          UBLFOA
     C                   Z-Add     KPos          UBLPOS
     C                   Z-Add     PUBLETQ       WUBLETQ          24 0
     C                   Z-Add     WUBLETQ       UBLETQ
     C                   If        LkCod = '7'
     C                   Move      '7'           UBLTIP
     C                   EndIf
     C                   If        Es24@A = 'S'
     C                   Move      WTi24         UBLTIE
     C                   EndIf
     C                   Move      WSe           UBLSEC
     C                   Move      WDo           UBLDOC
     C                   Move      WDD           DsDD
     C                   Move      WMM           DsMM
     C                   Z-Add     2000          DsAA
     C                   Z-Add     0             WAANume
     C                   Move      WAA           WAANume           2 0
     C                   Add       WAANume       DsAA
     C                   Move      DsDD          UBLDDD
     C                   Move      DsMM          UBLMMD
     C                   Move      DsAA          UBLAAD

     C                   TIME                    TIMSTM
     C                   EVAL      UBLANO = ANOSYS
     C                   EVAL      UBLMES = MESSYS
     C                   EVAL      UBLDIA = DIASYS
     C                   EVAL      UBLHOR = HORSYS
     C                   EVAL      UBLPAN = LkPanta
      *
     C                   If        LKUser <> *Blanks
     C                   Eval      UBLUSU  = LKUser
     C                   Else
     C                   Eval      UBLUSU  = USER
     C                   EndIf

     C                   Eval      UBLLI1  = *Blanks
     C                   Eval      UBLLI2  = *Blanks
     C                   Eval      UBLLI3  = *Blanks
     C                   Eval      UBLLI4  = *Zeros
     C                   Eval      UBLLI5  = *Zeros
     C                   Eval      UBLLI6  = *Zeros
     C                   Eval      UBLLI7  = *Zeros

     C                   Move      KCod          ULTCOD
     C                   Z-Add     KFoA          ULTFOA
     C                   Z-Add     KPos          ULTPOS

     C                   Z-Add     WUBLETQ       ULTCAS

     C                   EndSr
      ******************************************************
      *   Propone la siguiente Ubicacion
      ******************************************************
     C     ProponUbica   BEGSR

     C                   Z-Add     PUBLPOS       WPos              3 0

     C                   DoU       *In30 = *Off  Or
     C                             WPos  = 999

     C                   Move      PUBLCOD       KCod
     C                   Z-Add     PUBLFOA       KFoA
     C                   Z-Add     WPos          KPos
     C                   Add       1             KPos
     C                   Add       1             WPos
     C                   Exsr      LeeChAUBIBULT
     C                   Exsr      CompruAUBIBULT
     C                   If        *In30 = *Off
     C                   Z-Add     KPos          PUBLPOS
     C                   Exsr      IniPan2

      * Comprobacion de si existen ubicaciones con posicion superior a la
      * 1ª propuesta cuands se elige proponer ubicacion = 'S'

     C                   Exsr      MiraPosicion
     C                   EndIf
      *
     C                   EndDo

     C                   EndSr
      ******************************************************
      *   Inicializa PAN0R
      ******************************************************
     C     IniPan0R      BEGSR

      *Se Apaga  el *In76 para poder establecer filtros
     C                   SetOff                                       76
     C                   Move      'N'           PSIGUB
     C                   Move      'N'           PFILTR
     C                   Move      *Blanks       PERROR

     C                   EndSr
      ******************************************************
      *   Inicializa PAN1R
      ******************************************************
     C     IniPan1       BEGSR

     C                   Move      *Blanks       PERROR

     C                   Movel     *Blanks       PUBLCOD
     C                   Z-Add     *Zeros        PUBLFOA
     C                   Z-Add     *Zeros        PUBLPOS
     C                   Movel     *Blanks       PERROR
     C                   Movel     *Blanks       PERRO1

     C                   EndSr
      ******************************************************
      *   Inicializa PAN2R
      ******************************************************
     C     IniPan2       BEGSR

     C                   Exsr      CuentaBultos

     C                   Z-Add     WNBultos      NBUL
     C                   Z-Add     *Zeros        PUBLETQ

     C                   EndSr
      ******************************************************
      *   Inicializa PANFI Pantalla de FILTROS
      ******************************************************
     C     IniPanFi      BEGSR

      * Se enciende el *In78 para NO visualizar el filtro x BULTO
     C                   SetOn                                        78
     C                   Z-Add     *Zeros        PFTIDE
     C                   Z-Add     99999         PFTIHA
     C                   Z-Add     0             PFSECC
     C                   Z-Add     0             PFPAIS
     C                   Z-Add     0             PFBULT

     C                   Z-Add     PFSECC        WSeccDes
     C                   Z-Add     PFSECC        WSeccHas
     C                   Z-Add     PFTIDE        WTienDes
     C                   Z-Add     PFTIHA        WTienHas
     C                   Z-Add     PFPAIS        WPaisDes
     C                   Z-Add     PFPAIS        WPaisHas
     C                   Z-Add     0             WBultDes          5 0
     C                   Z-Add     99999         WBultHas          5 0

     C                   Move      *Blanks       PERROR

     C                   EndSr
      ******************************************************
      *   Leer CHAIN AUBIBULT
      ******************************************************
     C     LeeChAUBIBULT BEGSR

     C     KAubiBULT     Chain     AUBIBULT

     C                   EndSr
      ******************************************************
      *   Leer CHAIN AUBIBULT
      ******************************************************
     C     LeeChAUBIBUMA BEGSR

     C     KAubiBUMA     Chain     AUBIBUMA

     C                   EndSr
      ******************************************************
      *   Graba Ubicacion
      ******************************************************
     C     GrabarAUBIBULTBEGSR

     C                   Write     RUBL

     C                   Exsr      GrabaLogUbica

     C                   EndSr
      **====================================================================
      **   RECUPERA USUARIO
      **====================================================================
     C     RTVUSU        BEGSR
      **--------------------------------------------------------------------
     C                   CALL      'RTVJOBA'
     C                   Parm                    RTUSU            10
      **--------------------------------------------------------------------
     C                   ENDSR
      **====================================================================
      **====================================================================
      **   Prepara la etiqueta si es de 24
      **====================================================================
     C     PreparaEtq    BEGSR
      **
     C                   Exsr      IniCopy
     C                   Eval      Xetqu = *Blanks
     C                   MoveL     PUBLETQ       WChkEtq
     C                   If        W00 = *Zeros
     C                   MoveL     W22           XEtqu            24
     C                   Else
     C                   MoveL     PUBLETQ       XEtqu
     C                   EndIf
     C                   Exsr      Casi2422
     C                   If        Es24@A = 'S'
     C                   EXSR      Mover24
     C                   EndIf
      **--------------------------------------------------------------------
     C                   EndSr
      **====================================================================
      **   Inicializa los campos de la Copy   CAS24
      **====================================================================
     C     IniCopy       BEGSR
      **--------------------------------------------------------------------
      **
     C                   Eval      Tm24@A = *Blanks
     C                   Eval      Tm24@N = *Zeros
     C                   Eval      Ti24@A = *Blanks
     C                   Eval      Ti24@N = *Zeros
     C                   Eval      Se24@A = *Blanks
     C                   Eval      Se24@N = *Zeros
     C                   Eval      Do24@A = *Blanks
     C                   Eval      Do24@N = *Zeros
     C                   Eval      Fd24@A = *Blanks
     C                   Eval      Fd24@N = *Zeros
     C                   Eval      An24@A = *Blanks
     C                   Eval      An24@N = *Zeros
     C                   Eval      Me24@A = *Blanks
     C                   Eval      Me24@N = *Zeros
     C                   Eval      Di24@A = *Blanks
     C                   Eval      Di24@N = *Zeros
     C                   Eval      Tp24@A = *Blanks
     C                   Eval      Tp24@N = *Zeros
     C                   Eval      Te24@A = *Blanks
     C                   Eval      Te24@N = *Zeros
     C                   Eval      Dg24@A = *Blanks
     C                   Eval      Dg24@N = *Zeros
     C                   Eval      Et24@A = *Blanks
     C                   Eval      Et24@N = *Zeros
     C                   Eval      ES24@A = 'N'
      **--------------------------------------------------------------------
     C                   EndSr
      **====================================================================
      **   Mueve campos cuando la etiqueta es de 24
      **====================================================================
     C     Mover24       BEGSR
      **--------------------------------------------------------------------
      * Antigua Ds WCASIO
     C                   Eval      WCASIO = Et24@A
     C                   Eval      WTM    = Tm24@A
     C                   Eval      WTI24  = Ti24@A
     C                   Eval      WSE    = Se24@A
     C                   Eval      WDO    = Do24@A
     C                   Eval      WDD    = Di24@A
     C                   Eval      WMM    = Me24@A
     C                   Eval      WAA    = An24@A
     C                   Eval      WTP    = Tp24@A
     C                   Eval      WTE    = Te24@A
     C                   Eval      WDC    = Dg24@A
      **--------------------------------------------------------------------
     C                   EndSr
      ************************************************************
      **  Comprueba si la Casiopea es de 22 o 24
      ************************************************************
     C     Casi2422      BEGSR
      *----------------------------------------------------------
     C                   MoveL     XEtqu         LkEtq24o22       24

     C                   Call      'CAS2422'
     C                   Parm                    LkEtq24o22
     C                   Parm                    Prm24
     C                   Parm                    Prm22
      *----------------------------------------------------------
     C                   ENDSR
      *********************************************************************
      /FREE
       //***********************************************************
       // Consultas
       //***********************************************************
       Begsr Consultas;

         If CAMCUR = 'PFTIDE';
            Exsr ConsuTiendas;
      /FREE
     C                   Move      TiendaDevueltaPFTIDE
      /END-FREE
         EndIf;

         If CAMCUR = 'PFTIHA';
            TiendaDevuelta = *Blanks;
            Exsr ConsuTiendas;
      /FREE
     C                   Move      TiendaDevueltaPFTIHA
      /END-FREE
         EndIf;

         If CAMCUR = 'PFPAIS';
            Exsr ConsuPais;
      /FREE
     C                   Move      PaisDevuelto  PFPAIS
      /END-FREE
      /FREE
         EndIf;

       Endsr;
      /END-FREE
      *********************************************************************
      *   Consulta Tiendas
      *********************************************************************
     C     ConsuTiendas  Begsr

     C                   Move      *Blanks       Empres
     C                   Move      *Blanks       Selec
     C                   Move      '0'           Pcose
     C                   Move      *Blanks       PSeln
     C                   Move      *Blanks       TiendaDevuelta

     C                   Call      'CSETR9'
     C                   Parm                    Empres            7
     C                   Parm                    Selec             1
     C                   Parm                    PCose             1
     C                   Parm                    PSelN            10
     C                   Parm                    TiendaDevuelta    9

     C                   EndSr
      *********************************************************************
      *   Consulta Pais
      *********************************************************************
     C     ConsuPais     Begsr

     C                   Move      *Blanks       Empres
     C                   Move      *Blanks       Pacci
     C                   Move      '0'           PCose
     C                   Move      '0'           PCEE
     C                   Move      *Blanks       PSeln
     C                   Move      *Blanks       PaisDevuelto

     C                   Call      'CSEPAR'
     C                   Parm                    Empres            7
     C                   Parm                    Pacci             1
     C                   Parm                    PCose             1
     C                   Parm                    PCEE              1
     C                   Parm                    PSelN            10
     C                   Parm                    PaisDevuelto      4

     C                   EndSr
      ******************************************************
      *   ValPanFi   PAN1R
      ******************************************************
     C     ValPanFi      BEGSR

     C                   Setoff                                       30
     C                   Move      *Blanks       PERROR
      * Seccion
     C                   Exsr      ValidaSeccion
      * Tiendas
     C                   If        *In30 = *Off
     C                   If        PFTIDE <> *Zeros And
     C                             PFTIHA <> 99999
     C                   Exsr      ValidaTiendas
     C                   Else
     C                   Z-Add     0             WTienDes          5 0
     C                   Z-Add     99999         WTienHas          5 0
     C                   EndIf
      * Pais
     C                   If        *In30 = *Off
     C                   If        PFPAIS <> 0
     C                   Z-Add     PFPAIS        WPaisDes          4 0
     C                   Z-Add     PFPAIS        WPaisHas          4 0
     C                   Else
     C                   Z-Add     0             WPaisDes
     C                   Z-Add     9999          WPaisHas
     C                   EndIF
     C                   EndIF
     C                   EndIF

     C                   EndSr
      ******************************************************
      *   Valida  Seccion en los filtros
      ******************************************************
     C     ValidaSeccion BEGSR

     C                   If        PFSECC <> 0
     C                   If        PFSECC > 3
     C                   SetOn                                        30
     C                   MoveL     KMalSecc      PERROR
     C                   Else
     C                   Z-Add     PFSECC        WSeccDes          1 0
     C                   Z-Add     PFSECC        WSeccHas          1 0
     C                   EndIf
     C                   Else
     C                   Z-Add     0             WSeccDes
     C                   Z-Add     9             WSeccHas
     C                   EndIf

     C                   EndSr
      ******************************************************
      *   Valida  Tiendas en los filtros
      ******************************************************
     C     ValidaTiendas BEGSR

     C                   If        PFTIDE > PFTIHA
     C                   SetOn                                        30
     C                   MoveL     KMalTien      PERROR
     C                   Else
     C                   Z-Add     PFTIDE        WTienDes
     C                   Z-Add     PFTIHA        WTienHas
     C                   EndIf

     C                   EndSr
      ****************************************************************
      *   Filtros Ok Si hay filtros comprueba q la etiqueta los cumpla
      ****************************************************************
     C     FiltrosOk     BEGSR

     C                   SetOff                                       30

     C                   If        Es24@A = 'S'
     C                   Exsr      FiltrosOK24
     C                   EndIf

     C                   EndSr
      ****************************************************************
      *   Filtros Ok Si hay filtros comprueba q la etiqueta 24
      ****************************************************************
     C     FiltrosOk24   BEGSR

      * Seccion
     C                   If        Se24@N < WSeccDes   Or
     C                             Se24@N > WSeccHas
     C                   SetOn                                        30
     C                   MoveL     KMalSecc      PERROR
     C                   EndIf
      * Tienda
     C                   If        *In30 = *Off
     C                   If        Ti24@N < WTienDes   Or
     C                             Ti24@N > WTienHas
     C                   SetOn                                        30
     C                   MoveL     KMalTien      PERROR
     C                   EndIf
     C                   EndIf
      * Pais
     C                   If        *In30 = *Off
     C                   Z-Add     0             WSena18           9 0
     C/EXEC SQL
     C+     Select  SENA18
     C+                      Into  :WSena18
     C+                      From  EENTIDAD
     C+                      Where TIEND9 = :Ti24@N
     C/END-EXEC
     C                   If        WSena18 <  WPaisDes Or
     C                             WSena18 >  WPaisHas
     C                   SetOn                                        30
     C                   MoveL     KMalPais      PERROR
     C                   EndIf
     C                   EndIf

     C                   EndSr
      **************************************************************
      ** Comprueba el estado del Sql SQLSTT.
      **************************************************************
     C     EstadoSql     BEGSR
      ** --------------------------------------------------------------
     C                   Z-Add     0             K                 3 0
     C                   Z-Add     0             J                 3 0
      **
     C     *Loval        SetLl     FSQLSTT
     C                   Read      FSQLSTT                                88
     C     *In88         DowEq     '0'
     C     FSTIPO        IfEq      'F'
     C                   Add       1             K
     C                   Move      FSFETC        FETC(K)
     C                   EndIf
     C     FSTIPO        IfEq      'N'
     C                   Add       1             J
     C                   Move      FSFETC        NOFETC(J)
     C                   EndIf
     C                   Read      FSQLSTT                                88
     C                   EndDo
      ** --------------------------------------------------------------
     C                   Endsr
      ********************************************************************
      /FREE
      //*************************************************************
      //* Proceso de Grabacion/Modificacion/Anulacion Observaciones
      //*************************************************************
       Begsr Observaciones;

          PrmIA       = IA;
          PrmID       = ID;
          PrmCIN      = CIN;
          PrmNA       = NA;
          PrmUser     = LkUser;
          PrmPanta    = LkPanta;
          PrmTipUb    = LkCod  ;
          PrmNomUb    = LkNom  ;
          PrmCodUb    = PUBLCOD;
          PrmFoaUb    = %Char(PUBLFOA);
          PrmPosUb    = %Char(PUBLPOS);
          PrmEtqUb    = %Char(ULTCAS) ;
          PrmPgmO     = 'UBLPRE';

          CallP Prg_UBLOBS( PrmIA    : PrmID    : PrmCIN   : PrmNA    :
                            PrmUser  : PrmPanta : PrmTipUb : PrmNomUb :
                            PrmCodUb : PrmFoaUb : PrmPosUb : PrmEtqUb :
                            PrmPgmO   );

       Endsr;
     *---------------------------------------------------------------------
     *#UDL
     * Comprueba si el Cd puede ubicar etiquetas que estén leídas aunque
     * no estén Conformadas
     *---------------------------------------------------------------------
       BegSr Llama_KVINZ;

         SoN = 'N';
           Par_Clave = 'KVINZUBL  ';
           Par_Ent1  = Id;
           CallP Pgm_INZ018 (Par_Clave : Par_Ent1 : Par_Ent2  : Par_Ent3 :
                             Par_Ent4  : Par_Ent5 : Par_Ent6  : Par_Ent7 :
                             Par_Ent8  : Par_Ent9 : Par_Ent10 :
                             Par_Sal1  : Par_Sal2 : Par_Sal3  :
                             Par_Sal4  : Par_Sal5 : Par_Sal6  : Par_Sal7 :
                             Par_Sal8  : Par_Sal9 : Par_Sal10);
           SoN = Par_Sal1;
       EndSr;
     *---------------------------------------------------------------------
     *#UDL
     * MiraSiLeida = Comprueba si la etiqueta NO CONFORMADA ya está leída
     *---------------------------------------------------------------------
       BegSr MiraSiLeida;

         Clear WCuantos;
         If (SwEtqDevTie = *On And EtiquetaMultiseccion);
             WCuantos = %Int(EsLeidaDevval(XEtqu)); //Subproc: Chk existe en DEVVAL
         ENDIF;

         // Etiqueta Devolución Tienda
         If (SwEtqDevTie = *On And Not EtiquetaMultiseccion);
          Exec Sql
             Select Count(*) Into :WCuantos
                    From ADEVVAL
                    Where WETIPP = :WTippX  And
                          WETIEO = :WTienX  And
                          WETIPM = :WTipmX  And
                          WEANO  = :WAnoDX  And
                          WEMES  = :WMesDX  And
                          WEDIA  = :WDiaDX  And
                          WESECO = :WSeccX  And
                          WETESA = :WTesaX  And           -- Lgmt-3555
                          WEDOCU = :WDocuX
                    Fetch First 1 Row Only;
         EndIf;


         If WCuantos > 0;
          LeidaSN = 'S';
         EndIf;

       EndSr;
      //************************************************************
      // Graba el Log
      //************************************************************
      Begsr GrabaLogUbica;

             LOGIA  = Ia          ;
             LOGID  = Id          ;
             LOGCOD = UBLCOD      ;
             LOGFOA = UBLFOA      ;
             LOGPOS = UBLPOS      ;
             LOGETQ = UBLETQ      ;
             LOGTIP = UBLTIP      ;
             LOGPRO = 'ALTAUBICA ';
             LOGANG = UBLANO      ;
             LOGMEG = UBLMES      ;
             LOGDIG = UBLDIA      ;
             LOGHOG = UBLHOR      ;
             LOGUSG = UBLUSU      ;
             LOGPAG = UBLPAN      ;
             LOGPGM = 'UBLPRE'    ;

             Write RLOGUBI;

       Endsr;
        //--------------------------------------------------------------------//
        // SUBPROCEDIMIENTOS:                                                 //
        //--------------------------------------------------------------------//
        // Subproc: EsLeidaDevval....................................Lgmt-3555//
        // Verifica si se ha leído y esperan a procesar en DEVVAL.            //
        //--------------------------------------------------------------------//
        Dcl-Proc EsLeidaDevval;
           Dcl-Pi *N Ind;
              pEtiqueta Like(XEtqu) Const;
           End-Pi;

           //Variable local..............
           Dcl-S lCuantos Int(10) Inz(0);

           //Chk si se ha leido en DEVVAL
           Exec Sql
             Select 1 Into :lCuantos
               From AMVTDA  m
                 Inner Join
                    ADEVVAL d
               on (m.mvtipp, m.mvtieo, m.mvtipm, m.mvano, m.mvmes, m.mvdia,
                   m.mvseco, m.mvtems, m.mvdocu)
                   =
                   (d.wetipp, d.wetieo, d.wetipm, d.weano, d.wemes, d.wedia,
                    d.weseco, d.wetesa, d.wedocu)
               Where mvli34 = :pEtiqueta
                 Fetch First 1 Row Only;

             //Si encuentra >> ON
             Return (SQLSTATE = SQL_Encontrado);

        End-Proc;

      // Lgmt-3555----------------------------------------------------------//
      // Procedimiento: EsMultiseccion                                      //
      // Verifica si la etiqueta es multisección o multidocumento           //
      // -------------------------------------------------------------------//
        dcl-proc EsMultiseccion;
           dcl-pi *N Ind;
              pIA    char( 2) const;   // Parámetro Almacén
              pXEtqu char(24) const;   // Parámetro Etiqueta
           end-pi;

           // Declaración de variables locales.
           dcl-s TipoEtiqueta char( 1) inz('');
           dcl-s vResultado   ind;

           // Limpia variables y obtiene datos................
           Clear DsSecDocu;
           VerSeccionDocus(pIA:pXEtqu:TipoEtiqueta:DsSecDocu);

           // Asignación de EsMultiseccion.....................
           // Devuelve *ON: Si TipoEtiqueta coincide con alguna................
           Return (TipoEtiqueta= MultiSeccion Or TipoEtiqueta= MultiDocumento);

        end-proc;

     *---------------------------------------------------------------------
     *---------------------------------------------------------------------
     *--------------------------------------------------------------------- 