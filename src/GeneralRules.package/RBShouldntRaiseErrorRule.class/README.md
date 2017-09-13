#shouldnt:raise: is a rather tricky method, it is mostly used to make a
test "read" nicer, however it some severe drawbacks. Hence, it should only
be used in certain cases. Consider the following examples.

Example 1:
----------
	self
		shouldnt: [ 1 somethingNotUnderstood ]
		raise: MessageNotUnderstood.

In this particular case the expectations meet the result, the test fails with
an assertion failure since a MNU is raised.

Example 2:
----------
	self shouldnt: [ 1/0 ] raise: MessageNotUnderstood.

In this case the test will fail with a ZeroDivide. So in the negative case
#shouldnt:raise: is not very helpful. 

As a result, #shouldnt:raise: does not change much on the test outcome, the
statement itself produces the same failures. However there is the third and
most common use case.

Example 3:
----------
	self shouldnt: [ 1/0 ] raise: Error.

In this case the test fails, as expected, however not on a ZeroDivide but an
internal error message that shadows the real error. Using Error as argument for
#shouldnt:raise: shadows any possible error that might happen and thus should
be avoided.