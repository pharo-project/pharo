command tests
setUpPackagesMBCommandFooTests
	"self reset"
	
	self
		setUpPackageClassDefs: { #Object. #MBFooCommandTestA. #(). #(). #(). }
		methodDefs: #()
		named: 'MBFooCommandTestA-dkh.1'
		ancestors: #().
	self
		setUpPackageClassDefs: { #Object. #MBFooCommandTestB. #(). #(). #(). }
		methodDefs: #()
		named: 'MBFooCommandTestB-dkh.1'
		ancestors: #().
