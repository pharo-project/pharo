tests - comparing
testMatch

	#('**' 'f**'  'f**o' 'f*' '*f*' 'f#*' 'f##' '*oo' '#oo' '*o*' '#o#' '#o*' '*o#' 'fo*' 'fo#' '*foo*' '###' '*#'  'f*#' 'f*#o')
		do: [ :each | self assert: (each match: 'foo') ].
	#('bar' 'foo#' '#foo' '*foo#' '#foo*' '*bar*') do: [ :each |
		self deny: (each match: 'foo') ]