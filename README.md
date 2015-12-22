# qldeb

Transforms all of quicklisp in debian packages.

## Notable bugs

### Missing packages

Some packages currently can't be built:

- antik
- dcm
- cl-prevalence-test
- km
- pettomato-indexed-priority-queue

`dcm` and `pettomato-indexed-priority-queue` should be fixable,
`cl-prevalence-test` looks like a bug in quicklisp distribution, and
`antik` and `km` are because the release's archive is invalid.

That said, out of 3200 packages, having 5 missing packages is pretty
good.

### Out of heap memory

Currently, there's a bug with out of heap memory after building a
couple of thousands of packages. There needs to be some way to free
the data. (Or just increase sbcl's heap.)

# License

MIT License.
