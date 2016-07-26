launching
emergencyEvaluator
	"Display and launch an evaluator"
	
	(self newInFrame: (0@0 corner: 420@300))
		show: 'Type revert (without quote) to revert your last method change.
Type exit (without quote) to exit the emergency evaluator.';
		readEvalPrint.