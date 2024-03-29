WITH Ada.Text_IO;USE Ada.Text_IO;
WITH Ada.Integer_Text_IO; USE Ada.Integer_Text_IO;
WITH Ada.Numerics; USE Ada.Numerics;
With Ada.Numerics.Elementary_Functions; USE Ada.Numerics.Elementary_Functions;
with p_fenetre; use p_fenetre;
with Ada.Unchecked_Deallocation;

PACKAGE BODY P_Logo IS

   procedure build(t:in out turtle; interpreterRules:in out rules; op:in out pt_command) is
   BEGIN
      Op.Action:=(OTHERS=>' ');
      p_fenetre.Create_Graph_Window;
      t.orientation:=180;
      t.xCoord:=Get_window_Height/2;
      t.yCoord:=Get_window_Width/2;
      interpreterRules.penDown:=true;
   End build;
   -----------------
   -- Interpreter --
   -----------------

   PROCEDURE Interpreter IS
      Op:Pt_Command:=new t_command;
   interpreterRules:rules;
   turt:turtle;
   BEGIN
      Build(Turt,InterpreterRules,Op);
      While NOT compareStrings(Op.action,textToAction("exit")) LOOP
         op:=GetCommand;
         Put(op.action);
         Put(op.value);
         Put_line(" ");
         doCommand(Op,turt,interpreterRules);
         if inBufferCommands(op.action) then
            put_line("dealocatting buffer");
            --emptyBuffer(Op.buffer);
            put_line("done");
         end if;
      END LOOP;
   END Interpreter;

   --------------------
   -- CompareStrings --
   --------------------

   FUNCTION compareStrings (
         A,
         B : t_action)
     RETURN Boolean IS
   BEGIN
      return A=B;
   END compareStrings;

   ---------------
   -- getAction --
   ---------------

   function getCommand return pt_command is
   c : character;
   op:pt_command:=new t_command ;
   buff:pt_buffer;
   BEGIN
      op.action:=(others=>' ');
      get(c);
      while c=' ' OR c=';' loop
         get(c);
      end loop;
      for i in op.action'first..op.action'last loop
      Ada.Text_IO.put("got character:");
      put(c);
      Put_line(" ");
         if c=' ' OR c=';' then
         Ada.Text_IO.put_line("got space character");
            exit;
         end if;
         op.action(i):=c;
         get(c);
      end loop;
      Ada.Text_IO.put("got command:");
      p_logo.put(op.action);
      Put_line(" ");
      if NOT inNoValueCommands(op.action) then
         put_line("getting value");
         get(op.value);
      end if;
      if inBufferCommands(op.action) then
         buff:=getBufferCommand;
         op.buffer:=buff;
      end if;
      put_line("got whole command");
      return op;
   END getCommand ;

   function inNoValueCommands(action: t_action) return boolean is
   BEGIN
      IF compareStrings(action,textToAction("home"))
      OR compareStrings(action,textToAction("penup"))
      OR compareStrings(action,textToAction("pu"))
      OR compareStrings(action,textToAction("pendown"))
      OR compareStrings(action,textToAction("pd"))
      OR compareStrings(action,textToAction("clear"))
      OR compareStrings(action,textToAction("cs"))
      OR compareStrings(action,textToAction("["))
      OR compareStrings(action,textToAction("]"))
      then
          return true;
      end if;
      return false;
   end inNoValueCommands;

   function inBufferCommands(action: t_action) return boolean is
   BEGIN
      if compareStrings(action,textToAction("repeat")) then
          return true;
      end if;
      return false;
   end inBufferCommands;

   function getBufferCommand return pt_buffer is
   c:character;
   Command:Pt_Command:=NEW T_Command;
   CommandInBuffer:t_command_in_buffer;
   buffer:pt_buffer:=new t_command_buffer;
   BEGIN
   buffer.length:=0;
   command.action:=(others=>'#');
   get(c);
   while c=' ' loop
      get(c);
   end loop;
   if c='[' then
   Put_line("getting first command");
         Command:=GetCommand;
   Put_line("got first command");
   CommandInBuffer.command:=command;
      while not compareStrings(command.action,textToAction("]")) loop
         addToBuffer(buffer,CommandInBuffer);
         Put_line("getting next command");
         Command:=GetCommand;
         Put_line("got next command");
         CommandInBuffer.command:=command;
         END LOOP;
      END IF;
      return buffer;
   end getBufferCommand;

   PROCEDURE AddToBuffer(Buffer: IN OUT Pt_Buffer;Command: IN t_command_in_buffer) IS
      bufferCommand: pt_command_in_buffer:= new t_command_in_buffer;
      LastCommand:pt_Command_In_Buffer:= new t_command_in_buffer;
   BEGIN
      BufferCommand.Command:=Command.Command;
      BufferCommand.NextCommand:=Command.NextCommand;
   Put_line("adding command to buffer");
   if buffer.first=NULL then
   buffer.first:=bufferCommand;
      ELSE
         lastCommand:=LastCommandInBuffer(Buffer);
         lastCommand.nextCommand:=bufferCommand;
   end if;
   buffer.length:=buffer.length+1;
   listBufferCommands(Buffer);
   end addToBuffer;

   FUNCTION LastCommandInBuffer(Buffer:Pt_Buffer) RETURN pt_Command_In_Buffer IS
   command:pt_command_in_buffer:=buffer.first;
   BEGIN
   while command.nextCommand/=NULL loop
      command:=command.nextCommand;
   end loop;
   Put_line("got last command in buffer");
   return command;
   end lastCommandInBuffer;

   procedure listBufferCommands(Buffer:Pt_Buffer) IS
   command:pt_command_in_buffer:=buffer.first;
   BEGIN
   ada.Text_IO.put("buffer length:");
   put(buffer.length);
   put_line(" ");
   p_logo.put(command.command.action);
   ada.Text_IO.put_line(" ");
   while not endOfBuffer(buffer,command) loop
      p_logo.put(command.command.action);
      ada.Text_IO.put_line(" ");
      command:=command.nextCommand;
   end loop;
   put_line("end buffer list");
   End listBufferCommands;

   function endOfBuffer(buffer:Pt_Buffer;command:pt_command_in_buffer) return boolean is
   BEGIN
   return lastCommandInBuffer(buffer)=command;
   end endOfBuffer;

   function degreesToRad(d: Integer) return float is
   BEGIN
   return (Ada.Numerics.Pi*(float(d)))/float(180);
   end degreesToRad;

   function textToAction(S:in String) return t_action is
   action:t_action:=(others=>' ');
   BEGIN
      for i in S'range loop
         action(i):=S(i);
      end loop;
      return action;
   end textToAction;

   PROCEDURE emptyBuffer(Buff:Pt_Buffer) IS
      procedure free is new Ada.Unchecked_Deallocation(
         Object => t_command_in_buffer,
         Name   => pt_command_in_buffer);
   temp:pt_command_in_buffer;
   command:pt_command_in_buffer:=buff.first;
   BEGIN
      while not endOfBuffer(buff,command) loop
      temp:=command;
      if(command.command.buffer/=NULL) then
         emptyBuffer(command.command.buffer);
      end if;
      command:=command.nextCommand;
      free(temp);
      put_line("deallocation");
      end loop;
   end emptyBuffer;

   PROCEDURE put(act:in t_action) is
   BEGIN
      for i in act'range loop
         put(act(i));
      end loop;
   end put;

   procedure doCommand(comm:pt_command;turt:in out turtle;interpreterRules:in out rules) is
   BEGIN
   ada.Text_IO.put("doing command : ");
   p_logo.put(comm.action);
   put_line(" ");
      if compareStrings(comm.action,textToAction("forward")) OR compareStrings(comm.action,textToAction("fw")) then
         doForward(comm,turt,interpreterRules);
      elsif compareStrings(comm.action,textToAction("backward")) OR compareStrings(comm.action,textToAction("bk")) then
         doBackward(comm,turt,interpreterRules);
      elsif compareStrings(comm.action,textToAction("left")) OR compareStrings(comm.action,textToAction("lt")) then
         doLeft(comm,turt);
      elsif compareStrings(comm.action,textToAction("right")) OR compareStrings(comm.action,textToAction("rt")) then
         doRight(comm,turt);
      elsif compareStrings(comm.action,textToAction("repeat"))  then
         doRepeat(comm,turt,interpreterRules);
      elsif compareStrings(comm.action,textToAction("home")) then
         doHome(comm,turt);
      elsif compareStrings(comm.action,textToAction("penup")) OR compareStrings(comm.action,textToAction("pu")) then
         doPenUp(comm,interpreterRules);
      elsif compareStrings(comm.action,textToAction("pendown")) OR compareStrings(comm.action,textToAction("pd")) then
         doPenDown(comm,interpreterRules);
      elsif compareStrings(comm.action,textToAction("clear")) OR compareStrings(comm.action,textToAction("cs")) then
         doClear(comm);
      else
         put_line("Cette commande n'existe pas");
      end if;
    end doCommand;

   procedure doForward(comm:pt_command;turt:in out turtle;interpreterRules:in out rules) is
   oldX:integer:=turt.xCoord;
   oldY:integer:=turt.yCoord;
   BEGIN
   turt.xCoord:=turt.xCoord+integer(float(comm.value)*cos(degreesToRad(turt.orientation)));
   turt.yCoord:=turt.yCoord+integer(float(comm.value)*sin(degreesToRad(turt.orientation)));
   if Get_window_Height<turt.xCoord OR turt.yCoord<0 OR turt.xCoord<0 Or turt.yCoord>Get_window_Width then
      raise turtleOutsideWindow;
   end if;
   if interpreterRules.penDown then
      Draw_Line(oldY,oldX,turt.yCoord,turt.xCoord);
   end if;
   end doForward;

   procedure doBackward(comm:pt_command;turt:in out turtle;interpreterRules:in out rules) is
   oldX:integer:=turt.xCoord;
   oldY:integer:=turt.yCoord;
   BEGIN
      turt.xCoord:=(turt.xCoord-integer(float(comm.value)*cos(degreesToRad(turt.orientation))));
      turt.yCoord:=(turt.yCoord-integer(float(comm.value)*sin(degreesToRad(turt.orientation))));
      if Get_window_Height<turt.xCoord OR turt.yCoord<0 OR turt.xCoord<0 Or turt.yCoord>Get_window_Width then
      raise turtleOutsideWindow;
   end if;
      if interpreterRules.penDown then
         Draw_Line(oldY,oldX,turt.yCoord,turt.xCoord);
      end if;
   end doBackward;

   procedure doLeft(comm:in pt_command;turt:in out turtle)is
   BEGIN
   turt.orientation:=(turt.orientation+comm.value) mod 360;
   end doLeft;

   procedure doRight(comm:in pt_command;turt:in out turtle) is
   BEGIN
   turt.orientation:=(turt.orientation-comm.value) mod 360;
   end doRight;

   procedure doClear(comm:pt_command) is
   BEGIN
   Clear_Window;
   end doClear;

   procedure doHome(comm:pt_command;turt:in out turtle) is
   BEGIN
   turt.xCoord:=Get_window_Height/2;
   turt.yCoord:=Get_window_Width/2;
   end doHome;

   procedure doPenDown(comm:pt_command;interpreterRules:in out rules) is
   BEGIN
   interpreterRules.pendown:=true;
   end doPenDown;

   procedure doPenUp(comm:pt_command;interpreterRules:in out rules) is
   BEGIN
   interpreterRules.pendown:=false;
   end doPenUp;

   procedure doRepeat(comm:in pt_command;turt: in out turtle;interpreterRules:in out rules) IS
   BEGIN
   Put_line("repeating...");
   for i in 1..comm.value loop
      doBuffer(comm.buffer,turt,interpreterRules);
   end loop;
   end doRepeat;

   procedure doBuffer(buffer:in Pt_Buffer;turt:in out turtle;interpreterRules:in out rules ) IS
   currentCommand:pt_command_in_buffer:=buffer.first;
   BEGIN
   Put_line("doing first command");
      doCommand(currentCommand.command,turt,interpreterRules);
   while not endOfBuffer(buffer,currentCommand) loop
   Put_line("doing next command");
      currentCommand:=currentCommand.nextCommand;
      doCommand(currentCommand.command,turt,interpreterRules);
   end loop;
   end doBuffer;

   END P_Logo;
