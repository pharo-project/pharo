accessing
swap: r1 at: c1 with: r2 at: c2
	contents swap: (self indexForRow: r1 andColumn: c1)
			 with: (self indexForRow: r2 andColumn: c2)