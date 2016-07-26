tests
testPrintFormat

	| format monthTypes |
	format := DatePrintFormatTester on: january23rd2004.
	format dayPosition: 1 monthPosition: 2 yearPosition: 3 delimiter: $/ monthType: 1 yearType: 1.
	format shouldEqual: '23/1/2004'.
	
	format delimiter: $-.
	format shouldEqual: '23-1-2004'.
	
	format	monthType: 1 shouldPrintAs: 1 asString "index".
	format monthType: 2 shouldPrintAs: 'Jan'.
	format monthType: 3 shouldPrintAs: 'January'.
	
	"Out of range month types leave month blank"
	format monthType: 4.
	format shouldEqual: '23--2004' "no month".

	format	yearType: 1 shouldPrintAs: '2004'.
	format yearType: 2 shouldPrintAs: '04'.
	
	"Out of range year types -> short year"
	format yearType: 3 shouldPrintAs: '04'.
	
	"Usage examples"
	self 
		assert: june2nd1973 mmddyyyy = '6/2/1973';
		assert: june2nd1973 yyyymmdd = '1973-06-02';
		assert: (june2nd1973 printFormat: #(3 1 2 $! 2 1 1)) = '1973!2!Jun';
		assert: (june2nd1973 printFormat: #(1 2 3 0 1 1 2)) = '02061973';		
		assert: (june2nd1973 printFormat: #(2 1 3 0 1 1 2)) = '06021973';		
		assert: (june2nd1973 printFormat: #(3 2 1 0 1 1 2)) = '19730602';
		assert: (june2nd1973 printFormat: #(1 2 3 0 1 1 1)) = '261973'.