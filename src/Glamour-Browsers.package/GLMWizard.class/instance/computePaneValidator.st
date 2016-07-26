building internal
computePaneValidator

	|tmpPresentation tmpValidator|
	tmpPresentation := (self stepToUse presentationBlock glamourValue: self transmission transmissionStrategy presentations).
	tmpValidator := GLMValidator new.
	tmpValidator show: [:a | a custom: tmpPresentation]. 
	"tmpValidator startOn: self entity."
	^ tmpValidator 
