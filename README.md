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

`dcm`, `pettomato-indexed-priority-queue` and `cl-prevalence-test`
should be fixable, `antik` and `km` are because the release's
archive is invalid.

That said, out of 3200 packages, having 5 missing packages is fairly
OK.

### Out of heap memory

Currently, there's a bug with out of heap memory after building a
couple of thousands of packages. There needs to be some way to free
the data. (Or just increase sbcl's heap.)

# License

MIT License.
