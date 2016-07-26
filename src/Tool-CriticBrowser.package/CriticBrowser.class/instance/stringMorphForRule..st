private
stringMorphForRule: rule
	| unclassified falsePositives toDos text total |
	falsePositives := (cache falsePositiveOf: rule) size.
	toDos := (cache toDosOf: rule) size.
	total := (cache criticsOf: rule) size.
	unclassified := total - falsePositives - toDos.
	text := String
		streamContents: [ :s | 
			s
				<< rule name;
				<< ' (To sort: ';
				print: unclassified;
				<< ', ToDo: ';
				print: toDos;
				<< ', Wrong: ';
				print: falsePositives;
				<< ')' ].
	^ text asMorph
		color: (self colorForRule: rule);
		yourself
		