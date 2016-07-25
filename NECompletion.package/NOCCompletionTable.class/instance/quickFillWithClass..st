protocol
quickFillWithClass: cls
	cls methodsDo: [:e | self justCompiled: e selector in: cls]
	