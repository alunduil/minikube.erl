# Description

A manager for [minikube] VMs written in [Erlang]/[OTP].

This is best suited for quickly ensuring that a [kubernetes] cluster is
available during testing but can used for any purpose where a [minikube] VM
would be useful.

# Getting Started

Documentation is available on [hex] and [minikube] has its own help on the
command line (`minikube --help`).

This project is created with [rebar3] and the normal [rebar3] flow will
generally do what you need.

# Reporting Issues

Any issus discovered should be recorded on [github][issues].  If you believe
you've fuond an error or have a suggestion for a new feature; please ensure
that it is reported.

If you would like to contribute a fix or new feature; please submit a pull
request.  This project follows [git flow] and utilizes [travis] to
automatically check pull requests before a manual review.

[Erlang]: https://www.erlang.org
[git flow]: http://nvie.com/posts/a-successful-git-branching-model/
[hex]: https://hex.pm/packages/minikube
[issues]: https://github.com/alunduil/minikube.erl/issues
[kubernetes]: https://kubernetes.io
[minikube]: https://github.com/kubernetes/minikube
[OTP]: http://erlang.org/doc/design_principles/users_guide.html
[rebar3]: https://www.rebar3.org
[travis]: https://travis-ci.org/alunduil/minikube.erl
