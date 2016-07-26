pragma-selectors
after: aSelector
	<gtExamplePragma>
	<description: 'A method, provided by the given selector, to be performed after this example has run ~ similar but not identical to teardown of sunit'>
	
	after := self methodForSelector: aSelector