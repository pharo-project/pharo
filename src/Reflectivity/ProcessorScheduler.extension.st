Extension { #name : #ProcessorScheduler }

{ #category : #'*Reflectivity' }
ProcessorScheduler >> rfactiveProcess [
	"Answer the currently running Process."
	<metaLinkOptions: #( + optionDisabledLink)>
	^activeProcess rfeffectiveProcess
]
