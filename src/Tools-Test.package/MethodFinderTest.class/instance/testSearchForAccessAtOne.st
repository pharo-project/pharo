test examples
testSearchForAccessAtOne
	"self debug: #testSearchForAccessAtOne"
	
	self
		assert: (MethodFinder new load: #(('abcd' 1) $a  ('Ted ' 3) $d ))
			searchForOne asArray
		equals: #('data1 at: data2' 'data1 atPin: data2' 'data1 atWrap: data2')
		 