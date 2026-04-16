Class {
	#name : 'AnnouncementErrorController',
	#superclass : 'Object',
	#classInstVars : [
		'link',
		'active'
	],
	#category : 'Reflectivity-Tools-Announcements',
	#package : 'Reflectivity-Tools',
	#tag : 'Announcements'
}

{ #category : 'as yet unclassified' }
AnnouncementErrorController class >> disable [

	<script>
	self removeLink
]

{ #category : 'as yet unclassified' }
AnnouncementErrorController class >> makeAnnouncementUnhandledErrorsBlocking [
 
	<script>
	| ast |
	self removeLink.
	link := MetaLink new.
	ast := (BlockClosure >> #on:fork:) ast.
	link metaObject: [ :rcv :args |
		rcv
			on: args first
			do: args second ].
	link selector: #value:value:.
	link control: #instead.
	link arguments: #( receiver arguments ).
	ast link: link
]

{ #category : 'as yet unclassified' }
AnnouncementErrorController class >> removeLink [
	link ifNil:[^self].
	link uninstall
]
