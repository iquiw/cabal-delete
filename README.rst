===================
cabal-delete README
===================

Summary
-------
cabal-delete deletes installed directories of the specified package
and unregisters it if it has no reverse dependency and
it is not installed with ghc. 

cabal-delete checks reverse dependencies and installed directories
from package.conf (< 6.12) or package.cache (>= 6.12).

It cannot delete executable packages.

Usage
-----
To delete `package`, type "cabal-delete `package`".

If the package has no reverse dependency, message like the following is
displayed::

    $ cabal-delete primitive-0.3
    The following directories will be processed.
        D: Delete, N: NotFound, I: Ignore, A: Abort
    [D] /usr/local/ghc/pkg/lib/primitive-0.3
    [D] /usr/local/ghc/pkg/share/doc/primitive-0.3
    Do you want to delete primitive-0.3 ? [Y/n] 

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


The following options are available::

    usage: cabal-delete [option] [package...]

      -h  --help               show this help
      -i  --info               show package info
      -l  --multiple-versions  list packages with multiple versions
      -m  --multiple-minors    list packages with multiple minor versions
      -r  --reverse-depends    list packages with no reverse dependency
      -n  --dry-run            check what will happen without actual action
      -v  --version            show version number


Bug
___
If you use different version of ghc from one which cabal-delete was built by
and the ghc is installed under different directory, then cabal-delete cannot
detect abort (``[A]``) case.
