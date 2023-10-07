Extension { #name : 'Object' }

{ #category : '*Beacon-Core' }
Object >> asBeaconSignal [

	^ WrapperSignal on: self
]

{ #category : '*Beacon-Core' }
Object >> emit [
	"Convert the receiver as a beacon signal to the main signal dispatcher so that 
	 registered loggers get a chance to act on signal reception."

	Beacon announce: self asBeaconSignal
]
