Class {
	#name : 'EFFFICallTest',
	#superclass : 'EFTest',
	#category : 'EnlumineurFormatter-Tests',
	#package : 'EnlumineurFormatter-Tests'
}

{ #category : 'configurations' }
EFFFICallTest >> basicConfiguration [

	^ self contextClass new
]

{ #category : 'configurations' }
EFFFICallTest >> noNewLineBetweenTopCommentsConfiguration [
	"Here we can control explicitely the configuration we want."

	" String streamContents: [:s |
			EFContext new storeOn: s]"

	^ self contextClass basicNew
		  indentStyle: #tabulation;
		  formatCommentCloseToStatements: false;
		  useBasicCommentFormat: true;
		  numberOfNewLinesAfterMethodSignature: 1;
		  numberOfNewLinesAfterMethodSignatureWithMethodComment: 1;
		  numberOfNewLinesAfterMethodComment: 0;
		  indentExtraSpaces: 0
]

{ #category : 'tests' }
EFFFICallTest >> testOtherFFI [

	| expr source originalSource |
	originalSource := 'loadPath: aPath
	^ self ffiCall: #( void cairo_append_path (self , AthensCairoPath aPath))
	'.
	expr := OCParser parseMethod: originalSource.
	configurationSelector := #basicConfiguration.
	source := self newFormatter format: expr.
	self assert: source equals: originalSource
]

{ #category : 'tests' }
EFFFICallTest >> testOtherFFI2 [

	| expr source originalSource |
	originalSource := 'primGetCurrentPointX: x Y: y
	^ self ffiCall: #( void cairo_get_current_point (self, double * x, double * y))
	'.
	expr := OCParser parseMethod: originalSource.
	configurationSelector := #basicConfiguration.
	source := self newFormatter format: expr.
	self assert: source equals: originalSource
]

{ #category : 'tests' }
EFFFICallTest >> testOtherFFI3 [

	| expr source originalSource |
	originalSource := 'primResetDashes: anOffset
"	void                cairo_set_dash                      (cairo_t *cr,
                                                         double *dashes,
                                                         int num_dashes,
                                                         double offset);"
	^ self ffiCall: #(void cairo_set_dash (
				self,
				void* 0,
				int 0,
				double anOffset) )	'.
	expr := OCParser parseMethod: originalSource.
	configurationSelector := #basicConfiguration.
	source := self newFormatter format: expr.
	self assert: source equals: originalSource
]

{ #category : 'tests' }
EFFFICallTest >> testSimpleFFI [

	| expr source originalSource |
	originalSource := 'getAxis: axis
	^ self ffiCall: #( Sint16 SDL_GameControllerGetAxis(self , int axis) )
	'.
	expr := OCParser parseMethod: originalSource. 
	configurationSelector := #basicConfiguration.
	source := self newFormatter format: expr.
	self assert: source equals: originalSource
]

{ #category : 'tests' }
EFFFICallTest >> testSimpleFFISafelyOptions [

	| expr source originalSource |
	originalSource := 'blame_get_hunk_byindex: blame index: index
	"todo: using LGitBlameHunk* as return value causes random crashes"
	
	^ self
		ffiCallSafely: #(void * git_blame_get_hunk_byindex #(self , uint32 index))
		options: #()'.
	expr := OCParser parseMethod: originalSource. 
	configurationSelector := #basicConfiguration.
	source := self newFormatter format: expr.
	self assert: source equals: originalSource
]

{ #category : 'tests' }
EFFFICallTest >> testSimpleFFISafelyOptions2 [

	| expr source originalSource |
	originalSource := 'config_get_bool: name to: out

	^ self ffiCallSafely:
			#(LGitReturnCodeEnum git_config_get_bool(
				int *out,
				self, 
				String name))
		options: #()'.
	expr := OCParser parseMethod: originalSource. 
	configurationSelector := #basicConfiguration.
	source := self newFormatter format: expr.
	self assert: source equals: originalSource
]
