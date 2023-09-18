"
Unit tests for ZnPrefixMappingDelegate
"
Class {
	#name : #ZnPrefixMappingDelegateTest,
	#superclass : #TestCase,
	#category : #'Zinc-HTTP-Examples'
}

{ #category : #tests }
ZnPrefixMappingDelegateTest >> testMapping [
	| delegate handled |
	handled := false.

	delegate := ZnPrefixMappingDelegate new
		map: 'first' to: [ :req | handled := true ].

	delegate handleRequest: (ZnRequest empty url: '/first' asZnUrl).

	self assert: handled
]

{ #category : #tests }
ZnPrefixMappingDelegateTest >> testNesting [
	| delegate handled response |
	handled := false.

	delegate := ZnPrefixMappingDelegate new
		map: 'first' to: (ZnPrefixMappingDelegate new
			map: 'second' to: [ :req | handled := true ]).

	delegate handleRequest: (ZnRequest empty url: '/first/second' asZnUrl).
	self assert: handled.

	"In a nested scenario the outer prefix should not be found"
	response := delegate handleRequest: (ZnRequest empty url: '/first' asZnUrl).
	self assert: response status equals: 404
]

{ #category : #tests }
ZnPrefixMappingDelegateTest >> testPathConsumption [
	| delegate innerPath uri |
	uri := '/first/second/third' asZnUrl.

	delegate := ZnPrefixMappingDelegate new
		map: 'first' to: [ :req | innerPath := req url pathSegments ].

	delegate handleRequest: (ZnRequest empty url: uri).

	self assert: innerPath size equals: 2.
	self assert: uri pathSegments size equals: 3
]
