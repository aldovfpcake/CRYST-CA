SET TALK OFF
SET DATE italian
SET CENTURY ON
SET PROCEDURE TO c:\cryst-ca\cryclase
x = CREATEOBJECT("recibo")
x.mes=4
x.ano = 2017
x.abroarchivo
x.cursorpersonal
x.cursorliq
x.LLenoCursor
x.letramonto

PUBLIC obdaterec as Object
obdaterec = CREATEOBJECT("daterecib")

obdaterec.fechapago   = CTOD("30-04-2017")
obdaterec.fecpjub     = CTOD("08-03-2017")
obdaterec.banco       = "HSBC"
obdaterec.nombremes   = x.nombremes(x.mes)
obdaterec.año         = x.ano
obdaterec.lugaryfecha = "Capital Federal 30-04-2017"

*PUBLIC fechapago,ano,banco,fecpjub,nombremes
*fechapago = " "
*ano = 0
*banco = "HSBC"
*fecpjub = "08-04-2017"
*nombremes = "xxx"
SELECT V.LEGAJO as leg,V.NOMBRE,V.CATEGORIA as CATEGORIA,V.DEPART AS DEPART,;
V.CUIL AS cuil,V.TIPODOC AS TIPODOC,V.DOCUMENTO AS DOCUMENTO,V.FECHAING AS FECHAINGRE,;
V.CONTRATO AS CONTRATO,;
C.LEGAJO as legajo,C.CONCEPTO as concepto,C.DESCRIP as descrip,;
C.CANTIDAD as cantidad,C.APORTE AS aporte,C.SINAPORTE as sinaporte,C.DESCUENTO AS descuento,;
C.LETRANETO letraneto;
 FROM CURLIQ C INNER JOIN VPERSOLINEA AS V;
ON V.LEGAJO = C.LEGAJO  ORDER BY DEPART INTO CURSOR reporte

REPORT FORM report2 TO PRINTER PROMPT NODIALOG PREVIEW