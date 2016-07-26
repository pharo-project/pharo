tests
testFlip

	| r1 c r1Horiz r1Vert |
   r1 := 30@10 corner: 10@20.
   c := 5@5.
   r1Horiz := r1 flipBy: #horizontal centerAt: c.
   r1Vert := r1 flipBy: #vertical centerAt: c.

   self assert: r1 area = r1Horiz area description: 'flip preserves area'.
   self assert: r1 area = r1Vert area description: 'flip preserves area'.

 	self assert: r1 extent = r1Horiz extent description: 'flip preserves extent'.
	self assert: r1 extent = r1Vert extent description: 'flip preserves extent'.
	self assert:  (r1 flipBy: #horizontal centerAt: r1 center) = r1.
	self assert:  (r1 flipBy: #vertical centerAt: r1 center) = r1