Extension { #name : 'ProtoObject' }

{ #category : '*Instrumentation' }
ProtoObject >> insIsEqual: anObject [
	"I am a redefinition of the ProtoObject>>#== method. I have the same implementation of
	ProtoObject>>#== except for the instrumentation pragmas. I re-implement the method because
	when instrumenting I need to compare (see my senders) two numbers to control the
	concurrency. If the user instruments the method ProtoObject>>#== I will enter to an
	endless loop."

	<metaLinkOptions: #( #+ optionDisabledLink )>
	<noInstrumentation>
	<primitive: 110>
	self primitiveFailed
]
