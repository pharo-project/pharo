private
displayClass: aClassName protocol: aProtocolName
	"^ aClassName, '/protocols #', aProtocolName"
	"^ aClassName, ':::', aProtocolName"
	^ (self displayBehavior: aClassName), (self displayOperation: 'protocol:'), (self displayString: aProtocolName)
