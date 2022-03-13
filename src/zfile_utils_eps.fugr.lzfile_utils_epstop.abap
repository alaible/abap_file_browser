FUNCTION-POOL ZFILE_UTILS_EPS.              "MESSAGE-ID ..

* INCLUDE LZFILE_UTILS_EPSD...               " Local class definition

* global constants
DATA: GC_FTP_GET(3)     TYPE C                VALUE 'GET',
      GC_FTP_PUT(3)     TYPE C                VALUE 'PUT',
      GC_FTP_MGET(4)    TYPE C                VALUE 'MGET',
      GC_FTP_MPUT(4)    TYPE C                VALUE 'MPUT',
      GC_EPS_IN         LIKE EPSF-EPSSUBDIR   VALUE 'in',
      GC_EPS_OUT        LIKE EPSF-EPSSUBDIR   VALUE 'out',
      GC_EPS_LOG        LIKE EPSF-EPSSUBDIR   VALUE 'log',
      GC_TRANS_DIR      LIKE EPSF-EPSSUBDIR   VALUE '$TR_',
      GC_TRANS_DATA     LIKE EPSF-EPSSUBDIR   VALUE '$TR_DATA',
      GC_TRANS_COFILES  LIKE EPSF-EPSSUBDIR   VALUE '$TR_COFI',
      GC_TRANS_LOG      LIKE EPSF-EPSSUBDIR   VALUE '$TR_LOG',
      GC_TRANS_BUFFER   LIKE EPSF-EPSSUBDIR   VALUE '$TR_BUFF',
      GC_TRANS_TMP      LIKE EPSF-EPSSUBDIR   VALUE '$TR_TMP',
      GC_TRANS_BIN      LIKE EPSF-EPSSUBDIR   VALUE '$TR_BIN',
      GC_TRANS_SAPNAMES LIKE EPSF-EPSSUBDIR   VALUE '$TR_SAPN',
      GC_TRANS_ACTLOG   LIKE EPSF-EPSSUBDIR   VALUE '$TR_ACTL',
      GC_TRANS_OLDDATA  LIKE EPSF-EPSSUBDIR   VALUE '$TR_OLDD',
      GC_TRANS_SERIAL   LIKE EPSF-EPSSUBDIR   VALUE '$TR_SERI',
      GC_TRANS_BACKUP   LIKE EPSF-EPSSUBDIR   VALUE '$TR_BACK',
      GC_TRANS_API      LIKE EPSF-EPSSUBDIR   VALUE '$TR_API',
      GC_DEF_RECTRA     LIKE EPSF-EPSRECTRA   VALUE 10,
      GC_DEF_TXTRECTRA  LIKE EPSF-EPSRECTRA   VALUE 1000,
      GC_VERS_200       LIKE EPSFTPSI-VERSION VALUE '2.00',
      GC_VERS_202       LIKE EPSFTPSI-VERSION VALUE '2.02',
      GC_VERS_203       LIKE EPSFTPSI-VERSION VALUE '2.03',
      GC_OVRWRI_S       LIKE EPSF-EPSOVRWRI   VALUE 'S',
      GC_OVRWRI_F       LIKE EPSF-EPSOVRWRI   VALUE 'F',
      GC_OVRWRI_R       LIKE EPSF-EPSOVRWRI   VALUE 'R',
      GC_OVRWRI_A       LIKE EPSF-EPSOVRWRI   VALUE 'A', " append mode p.k. 2.8.2010
      GC_8000_BYTE      LIKE EPSF-EPSFILSIZ   VALUE 8000,
      GC_1000           TYPE I                VALUE 1000,
      GC_LAYOUT_1       TYPE C                VALUE '1',
      GC_LAYOUT_2       TYPE C                VALUE '2',
      GC_MON_FLAG_M     LIKE EPSF-EPSTRAMON   VALUE 'M'.