"
i am a concrete implementation of Athens surface which using cairo graphics library for rendering for generating PDF files
as output.

<currently it is not functional and need more testing and coding>
"
Class {
	#name : #AthensCairoPDFSurface,
	#superclass : #AthensCairoSurface,
	#category : #'Athens-Cairo-Surface'
}

{ #category : #private }
AthensCairoPDFSurface class >> createPDF: fileName width: width_in_points height: height_in_points [
	^ self ffiCall: #(AthensCairoPDFSurface cairo_pdf_surface_create (String fileName,
                                                      double width_in_points,
                                                      double height_in_points) )

]

{ #category : #'instance creation' }
AthensCairoPDFSurface class >> extent: anExtent fileName: fileName [

	^ (self createPDF: fileName width: anExtent x height: anExtent y) initialize
]

{ #category : #'finalize resources' }
AthensCairoPDFSurface class >> finalizeResourceData: handle [

	self destroySurfaceHandle: handle.

]

{ #category : #private }
AthensCairoPDFSurface class >> registerSurface: surface [
	"do not register my instances with SurfacePlugin"
]

{ #category : #'initialize-release' }
AthensCairoPDFSurface >> resourceData [
	^ handle value 
]

{ #category : #private }
AthensCairoPDFSurface >> showPage [
	^ self ffiCall: #( void cairo_surface_show_page (self) )
]
