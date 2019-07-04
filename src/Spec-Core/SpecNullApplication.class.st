"
I'm a default application for the case where no application is defined.
I'm meant to provide backwards compatibility and I SHOULD NOT BE USED REGULARLY.
"
Class {
	#name : #SpecNullApplication,
	#superclass : #SpecApplication,
	#category : #'Spec-Core-Base'
}

{ #category : #accessing }
SpecNullApplication class >> reset [
	<script>
	
	DefaultApplication := nil
]
