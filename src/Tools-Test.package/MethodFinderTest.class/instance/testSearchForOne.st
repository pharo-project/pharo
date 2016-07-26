test examples
testSearchForOne
	"self debug: #testSearchForOne"
	
	self
		assert: (MethodFinder new load: #(('abcd') $a  ('TedK') $T))
			searchForOne asArray
		equals: #('data1 first' 'data1 anyOne')
		 