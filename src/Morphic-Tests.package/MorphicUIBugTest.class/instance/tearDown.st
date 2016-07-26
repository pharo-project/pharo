running
tearDown
	"default. tests will add morphs to list. Teardown will delete."

	cases do: [ :each | each delete ]