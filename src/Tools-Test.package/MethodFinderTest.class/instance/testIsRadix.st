test examples
testIsRadix
	"self debug: #testIsRadix"
	
	self assert: ((MethodFinder new load: #((4 2) '2r100'   (255 16) '16rFF'    (14 8) '8r16')) searchForOne) asArray = #('data1 storeStringBase: data2')
		
		"Apparently the system should find :
		the #('data1 radix: data2' 'data1 printStringBase: data2' 'data1 storeStringBase: data2')"