evaluating-private
writeSetterSourceCode

	stream 
		cr;
		tab;
		nextPutAll: message selector allButLast;
		nextPutAll: ' := ';
		nextPutAll: argumentNames anyOne 