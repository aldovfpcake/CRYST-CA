SET EXCLUSIVE OFF
_screen.visible = .f.
LOCAL unidad
*WAIT WINDOW lcPath
ALuNidad = ADDBS(JUSTPATH(SYS(16,1))) 
unidad = SUBSTR(ALuNidad,1,2)
SET SYSMENU TO
*cam = unidad + "\CRYSTRE"
SET PATH TO C:\CRYST-CA
DO FORM vprer
READ events