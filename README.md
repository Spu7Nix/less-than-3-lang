# <3 lang

funny functional language

```
becomes_zero = .==0;
is_even = (%2) becomes_zero;

main = 11 is_even;

// 11 (%2 . ==0)
// 11 % 2 == 0
```

# Function results

| value      | function    | result                              | example                              |
| ---------- | ----------- | ----------------------------------- | ------------------------------------ |
| `number`   | `+number`   | their sum                           | `1+2 // 3`                           |
| `number`   | `-number`   | their difference                    | `1-2 // -1`                          |
| `number`   | `*number`   | their product                       | `1*2 // 2`                           |
| `number`   | `/number`   | their quotient                      | `1/2 // 0.5`                         |
| `number`   | `%number`   | their remainder                     | `11%2 // 1`                          |
| `val1`     | `<~val2`    | `val2`                              | `1<~2 // 2`                          |
| `val1`     | `==val2`    | `true` or `false`                   | `1==2 // false`                      |
| `number`   | `>number`   | `true` or `false`                   | `1>2 // false`                       |
| `number`   | `<number`   | `true` or `false`                   | `1<2 // true`                        |
| `number`   | `>=number`  | `true` or `false`                   | `1>=2 // false`                      |
| `number`   | `<=number`  | `true` or `false`                   | `1<=2 // true`                       |
| `any`      | `!=any`     | `true` or `false`                   | `1!=2 // true`                       |
| `string`   | `+string`   | concatenation                       | `"a"+"b" // "ab"`                    |
| `string`   | `*number`   | repetition                          | `"a"*3 // "aaa"`                     |
| `string`   | `/string`   | split                               | `"a,b,c"/"," // ("a", "b", "c")`     |
| `tuple`    | `+tuple`    | concatenation                       | `(1,2)+(3,4) // (1,2,3,4)`           |
| `tuple`    | `*number`   | repetition                          | `(1,2)*3 // (1,2,1,2,1,2)`           |
| `tuple`    | `-function` | filter                              | `(1,2,3,4,5) - (<3) // (3,4,5)`      |
| `tuple`    | `*function` | map                                 | `(1,2,3,4) * (*2) // (2,4,6,8)`      |
| `tuple`    | `/function` | reduce                              | `n add = +n; (1,2,3,4) / add // 10`  |
| `bool`     | `&bool`     | logical and                         | `true&false // false`                |
| `bool`     | ` \| bool`  | logical or                          | `true \| false // true`              |
| `function` | `+function` | composition                         | `(%2) + (==0) // "is even" function` |
| `function` | `*number`   | composed with itself n times        | `(%2)*3 // (%8)`                     |
| `int`      | `..int`     | range                               | `1..5 // (1,2,3,4)`                  |
| `function` | `:val`      | `val function` function application | `(*3):2 // 6`                        |
