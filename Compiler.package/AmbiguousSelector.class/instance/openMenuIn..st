handling
openMenuIn: aBlock 
	"Ask the user which selector to choose.
	Answer the choosen selector or nil if cancellation is requested."
	
	| labels actions lines caption choice |
	labels := {
		'Yes I want selector ' , name , ' with positive argument'.
		'Oops, I want ' , (name copyFrom: 1 to: name size - 1) , ' with negative argument'.
		'cancel, I must think twice'}.
	actions := {
		name.
		name copyReplaceFrom: name size to: name size - 1 with: ' '.
		nil.
	}.
	lines := {2}.
	caption := 'Ambiguous selector: ' , name , ' please correct, or cancel:'.
	choice := aBlock value: labels value: lines value: caption.
	self resume: (actions at: choice ifAbsent: [nil])