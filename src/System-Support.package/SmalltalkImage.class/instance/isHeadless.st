testing
isHeadless
	"Check if vm were run with headless parameter.
	Different VMs for different platform have different multiple way(s) to indicate that"
	
	-1000 to: 1000 do: [ :n | 
		(#('display=none' '-headless' '-vm-display-null' '-nodisplay' '--headless' '--vm-display-null' '--nodisplay') 
			includes: (self vm getSystemAttribute: n)) 
				ifTrue: [ ^ true ]].
	^ false
	
	
