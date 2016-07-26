visiting
acceptVisitor: anMTProjectVisitor
	anMTProjectVisitor visitProject: self.
	dependencies do: [ :dependency | dependency acceptVisitor: anMTProjectVisitor ]