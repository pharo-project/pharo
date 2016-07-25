private
basicHandle: aHandle at: index put: value
	value getHandle isExternalAddress 
		ifFalse: [ self error: 'value needs to be an ExternalAddress' ].
	^ aHandle pointerAt: index put: value getHandle
