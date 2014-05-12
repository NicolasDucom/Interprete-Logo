-----------------------------------------------------------------------
--
--  File:        adagraph.adb
--  Description: basic Win32 graphics
--  Rev:         0.7
--  Date:        march-2013
--  Author:      Jerry van Dijk
--  Mail:        jdijk@acm.org
--
--  Copyright (c) Jerry van Dijk, 1997 - 2003
--  Billie Hollidaystraat 28
--  2324 LK Leiden
--  THE NETHERLANDS
--  tel int + 31 71 531 4365
--
--  Permission granted to use for any purpose, provided this copyright
--  remains attached and unmodified.
--
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
--  IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
--  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
--
--  Correctif
--  Author:      B. Weinberg
--  Mail:        benjamin.roman.weinberg@gmail.com
--
-----------------------------------------------------------------------

with Interfaces.C, Ada.Numerics;

package body P_fenetre is

   pragma Linker_Options ("-lfenetre");


   --------------------------------
   -- Make the C types available --
   --------------------------------

   package C renames Interfaces.C;


   ----------------------------
   -- DLL Internal constants --
   ----------------------------

   No_Errors        : constant Integer := 0;
   MouseNone        : constant Integer := 0;
   MouseMove        : constant Integer := 1;
   MouseLeftUp      : constant Integer := 2;
   MouseRightUp     : constant Integer := 3;
   MouseLeftDown    : constant Integer := 4;
   MouseRightDown   : constant Integer := 5;


   ------------------------
   -- DLL Internal types --
   ------------------------

   type MouseEventStruct is
      record
         Event : Integer;
         Xpos  : Integer;
         Ypos  : Integer;
      end record;
   pragma Convention (C, MouseEventStruct);

   type MouseEventStructAccess is access all MouseEventStruct;
   pragma Convention (C, MouseEventStructAccess);

   type Integer_Access is access all Integer;
   pragma Convention (C, Integer_Access);

   type character_Access is access all CHARACTER;
   pragma Convention (C, character_Access);

   ----------------------
   -- Global variables --
   ----------------------

   ------------------------------
   -- Import the DLL functions --
   ------------------------------

   function GetKey (c : character_Access) return Integer;
   pragma Import (C, GetKey, "GetKey");

   function GetMouseLeftButtonDown (x, y : integer_Access) return Integer;
   pragma Import (C, GetMouseLeftButtonDown, "GetMouseLeftButtonDown");

   function GetMouseLeftButtonUp (x, y : integer_Access) return Integer;
   pragma Import (C, GetMouseLeftButtonUp, "GetMouseLeftButtonUp");

   function GetMouseRightButtonDown (x, y : integer_Access) return Integer;
   pragma Import (C, GetMouseRightButtonDown, "GetMouseRightButtonDown");

   function GetMouseRightButtonUp (x, y : integer_Access) return Integer;
   pragma Import (C, GetMouseRightButtonUp, "GetMouseRightButtonUp");

   function IsOpen return Integer;
   pragma Import (C, IsOpen, "IsOpen");

   function GetFontWidth return Integer;
   pragma Import (C, GetFontWidth, "GetFontWidth");

   function GetFontHeight return Integer;
   pragma Import (C, GetFontHeight, "GetFontHeight");

   function GetDllVersion return Integer;
   pragma Import (C, GetDllVersion, "GetDLLVersion");

   function GetWindowWidth return Integer;
   pragma Import (C, GetWindowWidth, "GetWindowWidth");

   function GetWindowHeight return Integer;
   pragma Import (C, GetWindowHeight, "GetWindowHeight");

   function CreateGraphWindow return Integer;
   pragma Import (C, CreateGraphWindow, "CreateGraphWindow");

   function DestroyGraphWindow return Integer;
   pragma Import (C, DestroyGraphWindow, "DestroyGraphWindow");

   function ClearWindow (Hue : Integer) return Integer;
   pragma Import (C, ClearWindow, "ClearWindow");

   function GetColorPixel (X, Y: Integer) return Integer;
   pragma Import (C, GetColorPixel, "GetColorPixel");

   function Putpixel (X, Y, Hue : Integer) return Integer;
   pragma Import (C, Putpixel, "PutPixel");

   function FillFlood (X, Y, Hue : Integer) return Integer;
   pragma Import (C, FillFlood, "FillFlood");

   function SetWindowTitle (Title : C.Char_Array) return Integer;
   pragma Import (C, SetWindowTitle, "SetWindowTitle");

   function DrawLine (X1, Y1, X2, Y2, Hue : Integer) return Integer;
   pragma Import (C, DrawLine, "DrawLine");

   function DrawBox (X1, Y1, X2, Y2, Hue, Filled : Integer) return Integer;
   pragma Import (C, DrawBox, "DrawBox");

   function DrawCircle (X, Y, Radius, Hue, Filled : Integer) return Integer;
   pragma Import (C, DrawCircle, "DrawCircle");

   function DrawArc (X, Y, a, b : INTEGER; debut, fin : FLOAT; Hue : Integer) return Integer;
   pragma Import (C, DrawArc, "DrawArc");

   function DrawEllipse (X1, Y1, X2, Y2, Hue, Filled : Integer) return Integer;
   pragma Import (C, DrawEllipse, "DrawEllipse");

   function DisplayText (X, Y : Integer; Text : C.Char_Array; Hue : Integer) return Integer;
   pragma Import (C, DisplayText, "DisplayText");

   function WhereX return Integer;
   pragma Import (C, WhereX, "WhereX");

   function WhereY return Integer;
   pragma Import (C, WhereY, "WhereY");

   function GotoXY (X, Y : Integer) return Integer;
   pragma Import (C, GotoXY, "GotoXY");

   function DrawTo (X, Y, Hue : Integer) return Integer;
   pragma Import (C, DrawTo, "DrawTo");


   ---------------------------------------------------
   -- Translate DLL error codes into Ada exceptions --
   ---------------------------------------------------

   procedure Make_Exception (Error : Integer) is
   begin
      if Error < No_Errors then
         case Error is
            when  -1    => raise Window_Already_Open;
            when  -2    => raise Window_Already_Closed;
            when  -3    => raise Message_Reg_Failed;
            when  -4    => raise Create_Event_Failed;
            when  -5    => raise Create_Thread_Failed;
            when  -6    => raise Window_Not_Open;
            when  -7    => raise Invalid_Color_Value;
            when  -8    => raise Invalid_Coordinate;
            when  -9    => raise Invalid_Fill_Option;
            when -10    => raise Create_Class_Failed;
            when -11    => raise Create_Window_Failed;
            when -12    => raise Error_Copying_Title;
            when -13    => raise Error_Copying_Cmdline;
            when -14    => raise Wait_Failed_Error;
            when -15    => raise Set_Title_Error;
            when -16    => raise Fill_Rect_Error;
            when -17    => raise Invalidate_Rect_Error;
            when -18    => raise Update_Window_Error;
            when -19    => raise Set_Pixel_Error;
            when -20    => raise Select_Pen_Error;
            when -21    => raise Move_To_Error;
            when -22    => raise Line_To_Error;
            when -23    => raise Select_Brush_Error;
            when -24    => raise Rectangle_Error;
            when -25    => raise Ellipse_Error;
            when -26    => raise Get_Pixel_Error;
            when -27    => raise Flood_Fill_Error;
            when -28    => raise Set_Textcolor_Error;
            when -29    => raise Text_Out_Error;
            when -30    => raise Invalid_Window_Size;
            when -31    => raise Get_Position_Error;
            when -32    => raise Close_Handle_Failed;
            when -33    => raise Thread_Status_Error;
            when others => raise Unknown_Adagraph_Error;
         end case;
      end if;
   end Make_Exception;


   ----------------------------
   -- Call the DLL functions --
   ----------------------------

   --------------------------------------------------------------------
   function Is_Open return Boolean is
      Result : Boolean;
   begin
      case IsOpen is
         when 0      => Result := False;
         when 1      => Result := True;
         when others => raise Unknown_Adagraph_Error;
      end case;
      return Result;
   end Is_Open;

   --------------------------------------------------------------------
   function Get_Dll_Version return Integer is
      Return_Value : Integer;
   begin
      Return_Value := GetDllVersion;
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      return Return_Value;
   end Get_Dll_Version;

   --------------------------------------------------------------------

   function Get_window_Width return INTEGER is
   begin
      return GetWindowWidth;
   end Get_window_Width;

   function Get_window_Height return INTEGER is
   begin
      return GetWindowHeight;
   end Get_window_height;

   function Get_Font_Width return INTEGER is
   begin
      return GetFontWidth;
   end Get_Font_Width;

   function Get_Font_Height return INTEGER is
   begin
      return GetFontHeight;
   end Get_Font_height;



   --------------------------------------------------------------------
   procedure Create_Graph_Window is
      Return_Value : Integer;
   begin
      Return_Value := CreateGraphWindow; -- (Default_Window);
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Create_Graph_Window;

   --------------------------------------------------------------------
   procedure Destroy_Graph_Window is
      Return_Value : Integer;
   begin
      Return_Value := DestroyGraphWindow;
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Destroy_Graph_Window;

   --------------------------------------------------------------------
   procedure Clear_Window (Hue : in Color_Type := Black) is
      Return_Value : Integer;
   begin
      Return_Value := ClearWindow (Color_Type'Pos (Hue));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Clear_Window;

   --------------------------------------------------------------------
   procedure Set_Window_Title(Title : in String) is
      Return_Value : Integer;
   begin
      Return_Value := SetWindowTitle (C.To_C (Title));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Set_Window_Title;

   --------------------------------------------------------------------
   function Get_Pixel (X, Y : Integer) return Color_Type is
      Return_Value : Integer;
   begin
      Return_Value := GetColorPixel (X, Y);
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      return Color_Type'Val (Return_Value);
   end Get_Pixel;

   --------------------------------------------------------------------
   procedure Put_Pixel (X, Y : in Integer; Hue : in Color_Type := White) is
      Return_Value : Integer;
   begin
      Return_Value := PutPixel (X, Y, Color_Type'Pos (Hue));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Put_Pixel;


   --------------------------------------------------------------------
   function Get_Key return Character is
      key : aliased CHARACTER;
      return_value : integer;
   begin
      return_value := GetKey(key'unchecked_access);
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      return key;
   end Get_Key;

   procedure Get_mouse_Left_button_down(X, Y : out INTEGER) is
      x_aux, y_aux : aliased INTEGER;
      return_value : integer;
   begin
      return_value := GetMouseLeftButtonDown(x_aux'unchecked_access, y_aux'unchecked_access);
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      X := X_Aux;
      y := Y_aux;
   end Get_mouse_Left_button_down;


   procedure Get_mouse_Left_button_up(X, Y : out INTEGER) is
      x_aux, y_aux : aliased INTEGER;
      return_value : integer;
   begin
      return_value := GetMouseLeftButtonUp(x_aux'unchecked_access, y_aux'unchecked_access);
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      X := X_Aux;
      y := Y_aux;
   end Get_Mouse_Left_Button_up;

   procedure Get_mouse_Right_button_down(X, Y : out INTEGER) is
      x_aux, y_aux : aliased INTEGER;
      return_value : integer;
   begin
      return_value := GetMouseRightButtonDown(x_aux'unchecked_access, y_aux'unchecked_access);
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      X := X_Aux;
      y := Y_aux;
   end Get_mouse_Right_button_down;


   procedure Get_mouse_Right_button_up(X, Y : out INTEGER) is
      x_aux, y_aux : aliased INTEGER;
      return_value : integer;
   begin
      return_value := GetMouseRightButtonUp(x_aux'unchecked_access, y_aux'unchecked_access);
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      X := X_Aux;
      y := Y_aux;
   end Get_Mouse_Right_Button_up;

   --------------------------------------------------------------------
   procedure Draw_Line (X1, Y1, X2, Y2 : in Integer;
                        Hue            : in Color_Type := White) is
      Return_Value : Integer;
   begin
      Return_Value := DrawLine (X1, Y1, X2, Y2, Color_Type'Pos (Hue));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Draw_Line;

   --------------------------------------------------------------------
   procedure Draw_Box (X1, Y1, X2, Y2 : in Integer;
                       Hue            : in Color_Type := White;
                       Filled         : in Fill_Type  := No_Fill) is
      Return_Value : Integer;
   begin
      Return_Value := DrawBox (X1, Y1, X2, Y2, Color_Type'Pos (Hue), Fill_Type'Pos (Filled));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Draw_Box;

   --------------------------------------------------------------------
   procedure Draw_Circle (X, Y, Radius : in Integer;
                          Hue          : in Color_Type := White;
                          Filled       : in Fill_Type  := No_Fill) is
      Return_Value : Integer;
   begin
      Return_Value := DrawCircle (X, Y, Radius, Color_Type'Pos (Hue), Fill_Type'Pos (Filled));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Draw_Circle;

   --------------------------------------------------------------------
   procedure Draw_Arc (X, Y, A, B : in Integer;
                        debut, fin : FLOAT;
                        Hue          : in Color_Type := White) is
      Return_Value : Integer;
   begin
      Return_Value := DrawArc (X, Y, a, b, debut, fin, Color_Type'Pos (Hue));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Draw_Arc;

   --------------------------------------------------------------------
   procedure Draw_Ellipse (X1, Y1, X2, Y2 : in Integer;
                           Hue            : in Color_Type := White;
                           Filled         : in Fill_Type  := No_Fill) is
      Return_Value : Integer;
   begin
      Return_Value := DrawEllipse (X1, Y1, X2, Y2, Color_Type'Pos (Hue), Fill_Type'Pos (Filled));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Draw_Ellipse;

   --------------------------------------------------------------------
   procedure Flood_Fill (X, Y : in Integer; Hue : in Color_Type := White) is
      Return_Value : Integer;
   begin
      Return_Value := FillFlood (X, Y, Color_Type'Pos (Hue));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Flood_Fill;

   --------------------------------------------------------------------
   procedure Display_Text (X, Y : in Integer;
                           Text : in String;
                           Hue  : in Color_Type := White) is
      Return_Value : Integer;
   begin
      Return_Value := DisplayText (X, Y, C.To_C (Text), Color_Type'Pos (Hue));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Display_Text;

   --------------------------------------------------------------------
   function Where_X return Integer is
      Return_Value : Integer;
   begin
      Return_Value := WhereX;
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      return Return_Value;
   end Where_X;

   --------------------------------------------------------------------
   function Where_Y return Integer is
      Return_Value : Integer;
   begin
      Return_Value := WhereY;
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
      return Return_Value;
   end Where_Y;

   --------------------------------------------------------------------
   procedure Goto_XY (X, Y : in Integer) is
      Return_Value : Integer;
   begin
      Return_Value := GotoXY (X, Y);
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Goto_XY;

   --------------------------------------------------------------------
   procedure Draw_To (X, Y : in Integer; Hue : in Color_Type := White) is
      Return_Value : Integer;
   begin
      Return_Value := DrawTo (X, Y, Color_Type'Pos (Hue));
      if Return_Value < No_Errors then
         Make_Exception (Return_Value);
      end if;
   end Draw_To;

   function Pi return FLOAT is
   begin
      return Ada.Numerics.Pi;
   end Pi;

begin

   -----------------------------------------------------
   -- Check if the right version of the DLL is loaded --
   -----------------------------------------------------

   if Get_Dll_Version < Adagraph_Dll_Version then
      raise Dll_Version_Error;
   end if;

end P_fenetre;
