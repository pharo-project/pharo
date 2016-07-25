private
runCase
	result ifNil: [ result := self classForTestResult new " fake for debug runner " ].
	super runCase. 