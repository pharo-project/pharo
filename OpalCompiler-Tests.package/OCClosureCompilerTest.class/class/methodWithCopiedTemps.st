code examples
methodWithCopiedTemps
	| a b c r |
	a := 1.
	b := 2.
	c := 4.
	r := [a + b + c] value.
	b := nil.
	r

	"Parser new
		parse: (self class sourceCodeAt: #methodWithCopiedTemps)
		class: self class"

	"(Parser new
		encoderClass: EncoderForV3;
		parse: (self class sourceCodeAt: #methodWithCopiedTemps)
		class: self class) generateUsingClosures: #(0 0 0 0)"