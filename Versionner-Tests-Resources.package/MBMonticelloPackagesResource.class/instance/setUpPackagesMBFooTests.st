ConfigurationOfMBFooTest
setUpPackagesMBFooTests
	"self reset"
	
	| versionInfo |
	versionInfo := self
		setUpPackageClassDefs: { #Object. #MBFooTestA. #(). #(). #(). }
		methodDefs: #()
		named: 'MBFooTests-dkh.1'
		ancestors: #().
	versionInfo := self 
		setUpPackageClassDefs: { #Object. #MBFooTestA. #(). #(). #(). }
		methodDefs: #()
		named: 'MBFooTests-dkh.2'
		ancestors: {versionInfo} .
	versionInfo := self 
		setUpPackageClassDefs: { #Object. #MBFooTestA. #(). #(). #(). }
		methodDefs: #(MBFooTestA foo 'foo ^self')
		named: 'MBFooTests-dkh.3'
		ancestors: {versionInfo} .
	versionInfo := self 
		setUpPackageClassDefs: { #Object. #MBFooTestA. #(). #(). #(). }
		methodDefs: #()
		named: 'MBFooTests-dkh.4'
		ancestors: {versionInfo} .
