
package p_logo is
	TYPE t_action is new String(1..20);

	--type noValueCommands is (home,penup,pu,pendown,pd,cg,clean);
	--type bufferCommands is (REPEAT);
	TYPE rules IS Record
		penDown:boolean;
	END RECORD;
	TYPE t_Command_Buffer;
	TYPE t_command_in_buffer;
	TYPE T_Command;
	TYPE pt_buffer is access all t_command_buffer;
	TYPE t_command IS Record
		action: t_action;
		value: Integer;
		buffer: pt_buffer;
    END RECORD;
    TYPE pt_command is access all t_command;
    TYPE pt_command_in_buffer is access all t_command_in_buffer;
	TYPE t_command_in_buffer is Record
		command: pt_command;
		nextCommand: pt_command_in_buffer;
	end record;
	TYPE t_command_buffer IS Record
		first:pt_command_in_buffer;
		length:Integer;
	end Record;

	TYPE turtle is Record
		orientation:Integer;
		xCoord:Integer;
		yCoord:Integer;
	end Record;

	turtleOutsideWindow: exception;

	procedure build(t:in out turtle; interpreterRules:in out rules; op:in out pt_command);
	Procedure interpreter;
	function compareStrings(A,B:t_action) return boolean;
	function getCommand return pt_command;
	function inNoValueCommands(action: t_action) return boolean;
	function inBufferCommands(action: t_action) return boolean;
	function getBufferCommand return pt_buffer;
	procedure addToBuffer(buffer: in out pt_buffer;command: in t_command_in_buffer);
	function lastCommandInBuffer(buffer:Pt_Buffer) return pt_command_in_buffer;
	procedure listBufferCommands(Buffer:Pt_Buffer);
	function endOfBuffer(buffer:Pt_Buffer;command:pt_command_in_buffer) return boolean;
	function degreesToRad(d: Integer) return float;
	function textToAction(S:String) return t_action;
	procedure emptyBuffer(buff:pt_buffer);
	procedure put(act:in t_action);
	procedure doCommand(comm:pt_command;turt:in out turtle;interpreterRules:in out rules);
	procedure doForward(comm:pt_command;turt:in out turtle;interpreterRules:in out rules);
	procedure doBackward(comm:pt_command;turt:in out turtle;interpreterRules:in out rules);
	procedure doLeft(comm:pt_command;turt:in out turtle);
	procedure doRight(comm:pt_command;turt:in out turtle);
	procedure doClear(comm:pt_command);
	procedure doHome(comm:pt_command;turt:in out turtle);
	procedure doPenDown(comm:pt_command;interpreterRules:in out rules);
	procedure doPenUp(comm:pt_command;interpreterRules:in out rules);
	procedure doRepeat(comm:in pt_command;turt: in out turtle;interpreterRules:in out rules);
	procedure doBuffer(buffer:in Pt_Buffer;turt:in out turtle;interpreterRules:in out rules);
	end p_logo;