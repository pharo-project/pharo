dct
idctBlockFloat: anArray component: aColorComponent 
	| t0 t1 t2 t3 t4 t5 t6 t7 t10 t11 t12 t13 z5 z10 z11 z12 z13 qt ws |
	qt := self qTable at: aColorComponent qTableIndex.
	ws := Array new: DCTSize2.

	"Pass 1: process columns from input, store into work array"
	1 
		to: DCTSize
		do: 
			[ :i | 
			t0 := (anArray at: i) * (qt at: i).
			t1 := (anArray at: DCTSize * 2 + i) * (qt at: DCTSize * 2 + i).
			t2 := (anArray at: DCTSize * 4 + i) * (qt at: DCTSize * 4 + i).
			t3 := (anArray at: DCTSize * 6 + i) * (qt at: DCTSize * 6 + i).
			t10 := t0 + t2.
			t11 := t0 - t2.
			t13 := t1 + t3.
			t12 := (t1 - t3) * DCTK1 - t13.
			t0 := t10 + t13.
			t3 := t10 - t13.
			t1 := t11 + t12.
			t2 := t11 - t12.
			t4 := (anArray at: DCTSize + i) * (qt at: DCTSize + i).
			t5 := (anArray at: DCTSize * 3 + i) * (qt at: DCTSize * 3 + i).
			t6 := (anArray at: DCTSize * 5 + i) * (qt at: DCTSize * 5 + i).
			t7 := (anArray at: DCTSize * 7 + i) * (qt at: DCTSize * 7 + i).
			z13 := t6 + t5.
			z10 := t6 - t5.
			z11 := t4 + t7.
			z12 := t4 - t7.
			t7 := z11 + z13.
			t11 := (z11 - z13) * DCTK1.
			z5 := (z10 + z12) * DCTK2.
			t10 := DCTK3 * z12 - z5.
			t12 := DCTK4 * z10 + z5.
			t6 := t12 - t7.
			t5 := t11 - t6.
			t4 := t10 + t5.
			ws 
				at: i
				put: t0 + t7.
			ws 
				at: DCTSize * 7 + i
				put: t0 - t7.
			ws 
				at: DCTSize + i
				put: t1 + t6.
			ws 
				at: DCTSize * 6 + i
				put: t1 - t6.
			ws 
				at: DCTSize * 2 + i
				put: t2 + t5.
			ws 
				at: DCTSize * 5 + i
				put: t2 - t5.
			ws 
				at: DCTSize * 4 + i
				put: t3 + t4.
			ws 
				at: DCTSize * 3 + i
				put: t3 - t4 ].

	"Pass 2: process rows from the workspace"
	(0 
		to: DCTSize2 - DCTSize
		by: DCTSize) do: 
		[ :i | 
		t10 := (ws at: i + 1) + (ws at: i + 5).
		t11 := (ws at: i + 1) - (ws at: i + 5).
		t13 := (ws at: i + 3) + (ws at: i + 7).
		t12 := ((ws at: i + 3) - (ws at: i + 7)) * DCTK1 - t13.
		t0 := t10 + t13.
		t3 := t10 - t13.
		t1 := t11 + t12.
		t2 := t11 - t12.
		z13 := (ws at: i + 6) + (ws at: i + 4).
		z10 := (ws at: i + 6) - (ws at: i + 4).
		z11 := (ws at: i + 2) + (ws at: i + 8).
		z12 := (ws at: i + 2) - (ws at: i + 8).
		t7 := z11 + z13.
		t11 := (z11 - z13) * DCTK1.
		z5 := (z10 + z12) * DCTK2.
		t10 := DCTK3 * z12 - z5.
		t12 := DCTK4 * z10 + z5.
		t6 := t12 - t7.
		t5 := t11 - t6.
		t4 := t10 + t5.

		"final output stage: scale down by a factor of 8 and range-limit"
		anArray 
			at: i + 1
			put: (self dctFloatRangeLimit: t0 + t7).
		anArray 
			at: i + 8
			put: (self dctFloatRangeLimit: t0 - t7).
		anArray 
			at: i + 2
			put: (self dctFloatRangeLimit: t1 + t6).
		anArray 
			at: i + 7
			put: (self dctFloatRangeLimit: t1 - t6).
		anArray 
			at: i + 3
			put: (self dctFloatRangeLimit: t2 + t5).
		anArray 
			at: i + 6
			put: (self dctFloatRangeLimit: t2 - t5).
		anArray 
			at: i + 5
			put: (self dctFloatRangeLimit: t3 + t4).
		anArray 
			at: i + 4
			put: (self dctFloatRangeLimit: t3 - t4) ]