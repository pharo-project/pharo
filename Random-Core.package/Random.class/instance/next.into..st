accessing
next: anInteger into: anArray
	1 to: anInteger do: [:index | anArray at: index put: self privateNextValue].
	^ anArray