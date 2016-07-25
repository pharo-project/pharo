private-rounds
rounds: data
	"Perform the four rounds with different functions"
	#(
	ffA:B:C:D:M:S:T:
	ggA:B:C:D:M:S:T:
	hhA:B:C:D:M:S:T:
	iiA:B:C:D:M:S:T:
	) doWithIndex: [ :selector :index |
		self round: data selector: selector round: index.]
