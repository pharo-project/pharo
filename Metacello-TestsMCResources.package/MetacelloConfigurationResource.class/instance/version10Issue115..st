Issue 115
version10Issue115: spec
	<version: '1.0' imports: #('1.0-baseline')>

	spec for: #'common' do: [
		spec blessing: #development.
		spec  project: 'Foo' .
		spec package: 'GeauxFoo' with: 'GeauxFoo-lr.1'. ].
