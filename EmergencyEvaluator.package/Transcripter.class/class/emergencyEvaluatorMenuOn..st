menu
emergencyEvaluatorMenuOn: aBuilder 
	<worldMenu> 
	
	(aBuilder item: #'Emergency Evaluator')
		action: [self emergencyEvaluator. World restoreMorphicDisplay];
		parent: #Tools;
		order: 0.57;	
		help: 'Invoke the emergency evaluator';
		icon: self icon
	