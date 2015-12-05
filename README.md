# qldeb

Transforms all of quicklisp in debian packages.

### Notes

- Use the dist-version as the version for all the packages, and make
  packages depend on each other using this specific version. This
  keeps the "distribution" way.
- Use cdebootstrap to build a new environment for the whole shebang to
  run.
