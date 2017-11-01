Extension { #name : #TextStyle }

{ #category : #'*Graphics-Fonts' }
TextStyle class >> pixelsPerInch: aNumber [
  "Set the nominal number of pixels per inch to aNumber."
  TextSharedInformation at: #pixelsPerInch put: aNumber asFloat.
  self changed: #textDPIChanged.

]
