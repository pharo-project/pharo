examples
exampleUseForm
" self exampleUseForm"
	| surf  form |
	
	form := Form extent: 100@100 depth: 32.
	form getCanvas fillRectangle: (0@0 corner: 30@30)  color: (Color red).
	form getCanvas fillRectangle: (10@10 corner: 40@40)  color: (Color green).
	form getCanvas fillRectangle: (20@20 corner: 50@50)  color: (Color blue).
	
	surf := AthensCairoSurface fromForm: form.
	Display getCanvas drawImage: surf asForm  at: 0@0
