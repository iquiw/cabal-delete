=====================
 cabal-delete README
=====================

.. contents:: Table of Contents
.. sectnum::

Summary
-------
cabal-delete deletes installed directories of the specified package
and unregisters it if it has no reverse dependency and
it is not installed with ghc.
It cannot delete executable packages.

cabal-delete also has some utility commands to show
package info, list packages multiple version installed,
and list packages with no reverse dependency.

Usage
-----

Delete packages
~~~~~~~~~~~~~~~
To delete `package`, type "cabal-delete `package`".
You may need `sudo` to delete directories `root` owns.

If the package has no reverse dependency, message like the following is
displayed::

    $ cabal-delete ghc-paths
    The following directories will be processed.
        D: Delete, N: NotFound, I: Ignore, A: Abort
    [D] /usr/local/ghc/pkg/lib/ghc-paths-0.1.0.8/ghc-7.0.4
    [D] /usr/local/ghc/pkg/share/doc/ghc-paths-0.1.0.8
    Do you want to delete ghc-paths-0.1.0.8 ? [Y]es, [N]o, [A]ll:

where the mark means;

- ``[D]`` means it will be deleted if '`Y`' or just enter key is pressed.
- ``[N]`` means it is found in package.conf (package.cache), but not exist.
- ``[I]`` means it does not contain package name.
  So it may be common directory, will be ignored.
- ``[A]`` means it is installed under the directory ghc installed.
  So delete process will be aborted.

If it has reverse dependencies, message like the following is displayed::

    $ cabal-delete parsec-2.1.0.1
    The follwoing packages depend on parsec-2.1.0.1
    vty-4.3.0.0
    regex-tdfa-1.1.2
    salvia-protocol-1.0.1


If `-R` option is specified, dependent packages will be delete recursively.

If `-n` option is specified, cabal-delete will not perform actual operation
(dry-run mode).

Show package information
~~~~~~~~~~~~~~~~~~~~~~~~
To show `package`'s information, type "cabal-delete -i `package`".

This shows name, description, dependencies and reverse dependencies
as follows::

    $ cabal-delete -i failure
    Name:            failure-0.0.0.3
    Description:     A simple type class for success/failure computations.
    Depends:         base-4.2.0.1
    ReverseDepends:  control-monad-failure-0.6.1

    Name:            failure-0.1.0
    Description:     A simple type class for success/failure computations.
    Depends:         base-4.2.0.1
    ReverseDepends:  control-monad-failure-0.7.0


Show packages that have multiple versions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To show packages with multiple versions installed,
type "cabal-delete -l"::

    $ cabal-delete -l
    The following packages have multiple versions.

    base                   : 3.0.3.2 4.2.0.1
    control-monad-failure  : 0.6.1 0.7.0
    data-accessor          : 0.2.1.2 0.2.1.3
    data-accessor-template : 0.2.1.3 0.2.1.4
    dyre                   : 0.8.2 0.8.3
    extensible-exceptions  : 0.1.1.1 0.1.1.2
    failure                : 0.0.0.3 0.1.0
    ...


To show packages with multiple minor versions installed,
type "cabal-delete -m".
`i.e.` Packages with same major version and different minor versions
installed::

    $ cabal-delete -m
    The following packages have multiple minor versions.

    data-accessor          : 0.2.1.2 0.2.1.3
    data-accessor-template : 0.2.1.3 0.2.1.4
    dyre                   : 0.8.2 0.8.3
    extensible-exceptions  : 0.1.1.1 0.1.1.2
    ...

Show packages that have no reverse dependency
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To show packages that have no reverse dependency, type "cabal-delete -r"::

    $ cabal-delete -r
    The following packages have no reverse dependency.

    BlazeHtml-0.1
    Imlib-0.1.2
    control-monad-failure-0.6.1
    control-monad-failure-0.7.0
    criterion-0.5.0.0
    data-accessor-template-0.2.1.4
    dph-par-0.4.0
    dph-seq-0.4.0
    dyre-0.8.3
    extensible-exceptions-0.1.1.1
    ...


Options
-------
The following options are available::

    usage: cabal-delete [option] [package...]

      -R  --recursive          delete packages recuresively
      -h  --help               show this help
      -i  --info               show package info
      -l  --multiple-versions  list packages with multiple versions
      -m  --multiple-minors    list packages with multiple minor versions
      -r  --reverse-depends    list packages with no reverse dependency
      -n  --dry-run            check what will happen without actual action
      -v  --version            show version number


Bug
---
* cabal-delete cannot distinguish if same packages (same version) are installed
  in both global and user's package databases.


TODO
----
1. Refactoring...

2. Do not perform recursive delete if any of dependent packages is ``[A]`` case.

3. `-y` option not to query user's answer (non-interactive mode).

4. Do not delete document directory if the library is installed for other GHC version also.

