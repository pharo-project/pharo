tests
testIfCompletelyBroken

	"If the results are not these values (accounting for precision of printing) 
	then something is horribly wrong"
	
	gen seed: 2345678901.
	self assert: (((1 to: 10) collect: [:i | gen next round: 15]) = #(0.149243269650845 0.331633021743797 0.75619644800024 0.393701540023881 0.941783181364547 0.549929193942775 0.659962596213428 0.991354559078512 0.696074432551896 0.922987899707159 )).	
