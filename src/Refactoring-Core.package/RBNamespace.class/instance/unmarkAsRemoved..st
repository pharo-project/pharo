private
unmarkAsRemoved: newClassName
	removedClasses
		remove: newClassName ifAbsent: [  ];
		remove: newClassName , ' class' ifAbsent: [  ]