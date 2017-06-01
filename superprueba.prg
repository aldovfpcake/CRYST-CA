       CLEAR
        NUM = 1678.75
        WAIT WINDOW MONTO(" ",NUM)
       
       
       
       
       
     
     	***************************
		FUNCTION MONTO
		************************

		parameters letras,NN


		DIMENSION NOMBRE [28]
		STORE "UNO"        TO NOMBRE[1]
		STORE "DOS"        TO NOMBRE [2]
		STORE "TRES"       TO NOMBRE [3]
		STORE "CUATRO"     TO NOMBRE [4]
		STORE "CINCO"      TO NOMBRE [5]
		STORE "SEIS"       TO NOMBRE [6]
		STORE "SIETE"      TO NOMBRE [7]
		STORE "OCHO"       TO NOMBRE [8]
		STORE "NUEVE"      TO NOMBRE [9]
		STORE "DIEZ"       TO NOMBRE [10]
		STORE "ONCE"       TO NOMBRE [11]
		STORE "DOCE"       TO NOMBRE [12]
		STORE "TRECE"      TO NOMBRE [13]
		STORE "CATORCE"    TO NOMBRE [14]
		STORE "QUINCE"     TO NOMBRE [15]
		STORE "DIECISEIS"  TO NOMBRE [16]
		STORE "DIECISIETE" TO NOMBRE [17]
		STORE "DIECIOCHO"  TO NOMBRE [18]
		STORE "DIECINUEVE "TO NOMBRE [19]
		STORE "VEINTE"     TO NOMBRE [20]
		STORE "TREINTA"    TO NOMBRE [21]
		STORE "CUARENTA "  TO NOMBRE [22]
		STORE "CINCUENTA"  TO NOMBRE [23]
		STORE "SESENTA  "  TO NOMBRE [24]
		STORE "SETENTA  "  TO NOMBRE [25]
		STORE "OCHENTA  "  TO NOMBRE [26]
		STORE "NOVENTA  "  TO NOMBRE [27]
		STORE "CIENTO   "  TO NOMBRE [28]






		STORE SPACE(20) TO GRACIA,NUNI,CENTENA,UNIMIL,PARCIAL,DECEMIL
		STORE SPACE(3) TO NRGRAL
		STORE SPACE(2) TO CC,DEC,UNID,VN,VMIL,WDEC
		STORE 0  TO CIENTOS,MIL,INTEGRO,DECI
		INTEGRO = INT(NN)
		DECI    = ((NN - INTEGRO)*100)

		IF DECI = 0
		   PARCIAL = ' '
		ELSE
		   PARCIAL = LTRIM("CON " + STR(DECI,2) +"/100")
		ENDIF

		IF INTEGRO < 10000
		   NRGRAL  = STR(INTEGRO,4)
		ELSE 
		   NRGRAL   = STR(INTEGRO,5)
		ENDIF

		IF INTEGRO < 10000
			CIENTOS = LEN(LTRIM(NRGRAL))
			MIL     = LEN(LTRIM(NRGRAL))
			CC      =  SUBSTR(NRGRAL,3)
			DEC     = SUBSTR(CC,1,1)
			UNID    = SUBSTR(CC,2)
			WDEC    = SUBSTR(CC,1,2)
		ELSE
		    CIENTOS  = VAL (SUBSTR (NRGRAL,3,1))
		    DEC      =  SUBSTR(NRGRAL,4,1)  
		    UNID     =  SUBSTR(NRGRAL,5,1)
		    WDEC     =  SUBSTR(NRGRAL,4,2)
		ENDIF

		IF INTEGRO >= 10000 .AND. INTEGRO < 20000
		    DECENAMIL = SUBSTR(NRGRAL,1,2)
		   	DECEMIL    =  NOMBRE[VAL(DECENAMIL)] + "MIL"
		ELSE
		    IF INTEGRO >=20000
		    	*WAIT WINDOW "SUBSTR-->" + SUBSTR(NRGRAL,1,1)+ "0"
            	DECENAMIL  = SUBSTR(NRGRAL,1,1)+ "0"		 
  	        	 	        	 	        	
  	        	UNIDADEMIL = " "
  	        	BNUM       = SUBSTR(NRGRAL,2,1)
  	        	UNIDADEMIL = NOMBRE[VAL(BNUM)]
  	        	IF DECENAMIL = "20"
  	        	   DECEMIL = "VEINTI"+ UNIDADEMIL + "MIL" 
  	        	ELSE
  	        	  DECEMIL    =  NOMBRE[VAL(DECENAMIL)]+ UNIDADEMIL + "MIL"   
		    	ENDIF
		    	MIL = 0
		    ENDIF
		    *WAIT WINDOW DECEMIL
		ENDIF



		IF MIL = 4
		   VMIL= SUBSTR(NRGRAL,1,1)
		   DO CASE
		      CASE VAL(VMIL) = 1
		             UNIMIL  = "MIL"
		      CASE VAL(VMIL) = 2
		           UNIMIL    = NOMBRE[VAL(VMIL)] + "MIL"  
		      CASE VAL(VMIL) = 3
		           UNIMIL    = NOMBRE[VAL(VMIL)] + "MIL"
		      CASE VAL(VMIL) = 4
		           UNIMIL    = NOMBRE[VAL(VMIL)] + "MIL"
		      CASE VAL(VMIL) = 5
		           UNIMIL    = NOMBRE[VAL(VMIL)] + "MIL"
		      CASE VAL(VMIL) = 6
		           UNIMIL    = NOMBRE[VAL(VMIL)] + "MIL"
		      CASE VAL(VMIL) = 7
		           UNIMIL    = NOMBRE[VAL(VMIL)] + "MIL"
		      CASE VAL(VMIL) = 8
		           UNIMIL    = NOMBRE[VAL(VMIL)] + "MIL"
		      CASE VAL(VMIL) = 9
		           UNIMIL    = NOMBRE[VAL(VMIL)] + "MIL"
		      OTHERWISE
		           UNIMIL    = ' '
		   ENDCASE
		ENDIF

		IF CIENTOS = 4 .OR. CIENTOS = 3
		   VN = RIGHT((SUBSTR(NRGRAL,1,2)),1)
		*  @1,1 SAY VN
		 DO CASE
		       CASE VAL(VN) = 0
		            CENTENA = '     '
		       CASE VAL(VN) = 1
		            IF VAL(WDEC) = 0
		               CENTENA = "CIEN"
		            ELSE
		               CENTENA = "CIENTO"
		            ENDIF
		        CASE VAL(VN) = 5
		            CENTENA = "QUINIENTOS"
		       CASE VAL(VN) = 9
		            CENTENA = "NOVECIENTOS"
		       OTHERWISE
		             CENTENA =  NOMBRE[VAL(VN)] + "CIENTOS"
		    ENDCASE
		ENDIF

		IF INTEGRO > 10000
		    DO CASE 
		        CASE CIENTOS = 0
		             CENTENA = " "    
		        CASE CIENTOS = 1
		             CENTENA = "CIENTO "    
		        CASE CIENTOS = 5
		             CENTENA = "QUINIENTOS"
		        CASE CIENTOS = 5      
		             CENTENA = "NOVECIENTOS"
		        OTHERWISE		 
		      		 CENTENA =  NOMBRE[CIENTOS] + "CIENTOS"
		    ENDCASE
		ENDIF




		DO CASE
		   CASE VAL(DEC) = 0
		        GRACIA = "        "
		   CASE VAL(DEC)= 1
		          GRACIA = NOMBRE[VAL(WDEC)]
		   CASE VAL(DEC) = 2
		        GRACIA   = NOMBRE[(VAL(DEC)*10)]
		   CASE VAL(DEC)= 3
		        GRACIA=    NOMBRE[(VAL(DEC)*10)-9]
		   CASE VAL(DEC)=4
		        GRACIA=    NOMBRE[(VAL(DEC)*10)-18]
		   CASE VAL(DEC)=5
		        GRACIA   = NOMBRE[(VAL(DEC)*10)-27]
		   CASE VAL(DEC)=6
		        GRACIA=    NOMBRE[(VAL(DEC)*10)-36]
		   CASE VAL(DEC)=7
		        GRACIA =   NOMBRE[(VAL(DEC)*10)-45]
		   CASE VAL(DEC)=8
		        GRACIA=    NOMBRE[(VAL(DEC)*10)-54]
		   CASE VAL(DEC)=9
		        GRACIA =   NOMBRE[(VAL(DEC)*10)-63]
		ENDCASE
		IF VAL(UNID) = 0 .OR. VAL(DEC)=1
		    NUNI = '  '
		ELSE
		   IF VAL(DEC)=0
		      NUNI = NOMBRE[VAL(UNID)]
		   ELSE
		     NUNI ="Y"+ " "+NOMBRE[VAL(UNID)]
		   ENDIF
		ENDIF


		IF INTEGRO < 10000
		    letras= TRIM(UNIMIL)+" "+TRIM(CENTENA)+" "+TRIM(GRACIA)+" "+TRIM(NUNI)+" "+RTRIM(PARCIAL)
		ELSE
		    IF INTEGRO > 10000
		        letras= TRIM(DECEMIL)+" "+TRIM(CENTENA)+" "+TRIM(GRACIA)+" "+TRIM(NUNI)+" "+RTRIM(PARCIAL)
		    ENDIF
		ENDIF    


		return letras