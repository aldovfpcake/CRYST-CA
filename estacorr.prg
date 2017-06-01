#DEFINE WM_CLOSE 0x10
clear
DECLARE LONG FindWindow IN WIN32API STRING ClassName, STRING WindowTitle
DECLARE LONG SendMessage IN WIN32API LONG HWND, LONG Msg, LONG wParam, LONG LPARAM

lcWindowTitle = "Sin título: Bloc de notas"

HWND = FindWindow( NULL, lcWindowTitle )
IF HWND = 0
  ? "Ventana no encontrada"
  RETURN
ENDIF
=SendMessage( HWND, WM_CLOSE, 0, 0 )
RETURN
