accessing
certificateCheck: aCallback
	certificateCheckCallback := aCallback.
	self prim_certificate_check: aCallback