Extension { #name : 'Behavior' }

{ #category : '*Deprecated14-Kernel' }
Behavior >> addSelector: selector withMethod: compiledMethod [

	<reflection: 'Class structural modification - Selector/Method modification'>
	self deprecated: 'Use #methodInstaller instead'.

	"This method installer does not announce"
	self methodInstaller
		logged: false;
		changeStamp: DateAndTime now;
		install: compiledMethod into: self compiler: nil
]
