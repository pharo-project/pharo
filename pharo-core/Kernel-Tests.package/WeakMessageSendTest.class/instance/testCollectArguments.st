as yet unclassified
testCollectArguments

	"Fogbugz issue 18127"
	"Test is passed if no primitve fails"
	
	| msg rcvr c|
	
	rcvr := OrderedCollection new.
	c := Color green.
	msg := WeakMessageSend receiver: rcvr selector: #add: arguments: { Color black }.
	
	msg valueWithArguments: {}.
	
	self assertCollection: rcvr asArray equals: { Color black }.
	
	msg valueWithArguments: { #a . WeakArray with: c }.
	
	self assertCollection: rcvr asArray equals: { Color black . #a }.
	
	msg valueWithArguments: (WeakArray with: c).
	
	self assertCollection: rcvr asArray equals: { Color black . #a  . Color green }.