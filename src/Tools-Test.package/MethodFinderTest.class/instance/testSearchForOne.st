test examples
testSearchForOne
	"self debug: #testSearchForOne"
	
	self
		assert: (MethodFinder new load: #(('abcd') $a  ('TedK') $T))
			searchForOne asSortedCollection asArray
		equals: #('data1 anyOne' 'data1 first')
		 