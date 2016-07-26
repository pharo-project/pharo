private
reapplyAllRules
	
	|  ruleCount total  rule |
	ruleCount := 0.
	rule := rulesModel roots first.
	total := rule leaves size.

	 rule leaves do: [ :r |
		ruleCount := ruleCount + 1.
		self setTitle: (String streamContents: [:s | s << 'run rules: ' << r name << ' (' << ruleCount asString << '/' << total asString <<')']).
		self reapplyRule: r ]. 	
	self setTitle: self defaultTitle.