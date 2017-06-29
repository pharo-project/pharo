as yet unclassified
testCollectArguments

	"Fogbugz issue 18127"
	"Test is passed if no primitve fails"
	
	| msg rcvr c|
	
	rcvr := OrderedCollection new.
	c := #( 42 ).
	msg := WeakMessageSend receiver: rcvr selector: #add: arguments: { 4 }.
	
	msg valueWithArguments: {}.
	
	self assertCollection: rcvr asArray equals: { 4 }.
	
	msg valueWithArguments: { #a . WeakArray with: c }.
	
	self assertCollection: rcvr asArray equals: { 4 . #a }.
	
	msg valueWithArguments: (WeakArray with: c).
	
	self assertCollection: rcvr asArray equals: { 4 . #a  . #( 42 ) }.