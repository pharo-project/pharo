Extension { #name : #MCConfiguration }

{ #category : #'*MonticelloGUI' }
MCConfiguration >> browse [

	^ (MCConfigurationBrowser new configuration: self) show
]
