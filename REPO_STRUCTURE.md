# Структура репозитория Probability-Workshops

> Этот файл описывает назначение каждой папки и файла.  
> Скопируйте содержимое архива в свой локальный репозиторий, переименуйте `.R`-файлы семинаров по шаблону ниже и сделайте `commit` через GitKraken.

```
Probability-Workshops/
├── README.md                          # Главная страница репозитория
├── index.md                           # Оглавление курса и навигация
├── .gitignore                         # Исключения для Git (R, OS)
├── REPO_STRUCTURE.md                  # Этот файл
│
├── seminars/                          # Семинары по теории вероятностей
│   ├── seminar-04-event-algebra.R
│   ├── seminar-05-intro-to-probability.R
│   ├── seminar-06-combinatorics.R
│   ├── seminar-06a-geometric-probability.R
│   ├── seminar-07-conditional-probability.R
│   ├── seminar-08-pmf-cdf.R
│   ├── seminar-09-bernoulli-binomial.R
│   ├── seminar-10-hypergeometric-poisson.R
│   ├── seminar-11-covariance-correlation.R
│   ├── seminar-12-continuous-random-variables.R
│   ├── seminar-13-...R
│   ├── seminar-14-jointly-distributed-discrete.R
│   ├── seminar-15-...R
│   └── seminar-17-central-limit-theorem.R
│
├── tests/                             # Зачёты и экзамены (без персональных данных)
│   ├── autumn-exam-variant-05.R
│   ├── autumn-exam-variant-...R
│   ├── midterm-01-example.R
│   └── midterm-02-example.R
│
├── glossary/                          # Словарь терминов (осенний семестр)
│   └── autumn-term-glossary.md
│
├── code/                              # Шпаргалки по распределениям и функциям R
│   ├── basic-discrete-distributions.R
│   ├── basic-continuous-distributions.R
│   ├── combinatorics-and-integration.R
│   └── portfolio-statistics.R
│
└── resources/                         # Внешние ресурсы, записи, литература
    ├── records-and-links.md
    └── creative-topics.md
```

## Правила именования файлов

- Используйте **строчные буквы** и **дефисы** вместо пробелов.
- **Не используйте** имена студентов в названиях файлов.
- Нумерация семинаров двузначная (04, 05, ..., 17) для правильной сортировки.

## Как загрузить через GitKraken (сначала в ветку, потом в main)

### Шаг 1. Подготовка файлов
1. Распакуйте архив в папку локального репозитория.
2. Переименуйте свои `.R`-файлы по шаблону выше.
3. Распределите файлы по папкам (`seminars/`, `tests/`).

### Шаг 2. Создание ветки
1. В GitKraken убедитесь, что вы на ветке `main`.
2. Нажмите кнопку **Branch** (иконка ветки) → введите имя новой ветки, например `restructure`.
3. GitKraken автоматически переключится на новую ветку.

### Шаг 3. Commit в ветку
1. В правой панели (Commit Panel) вы увидите все изменённые файлы.
2. Нажмите **Stage all changes** (или выберите файлы по одному).
3. Введите сообщение коммита, например:
   ```
   restructure: organize files, add README and glossary, remove personal data
   ```
4. Нажмите **Commit**.

### Шаг 4. Push ветки на GitHub
1. Нажмите **Push** — ветка `restructure` появится на GitHub.

### Шаг 5. Merge в main
**Вариант A — через GitKraken (merge):**
1. Переключитесь обратно на ветку `main` (двойной клик по `main` в левой панели).
2. Правый клик по ветке `restructure` → **Merge restructure into main**.
3. Нажмите **Push**.

**Вариант B — через Pull Request на GitHub (рекомендуется):**
1. Откройте репозиторий на GitHub.
2. Появится уведомление «Compare & pull request» для ветки `restructure`.
3. Нажмите **Create pull request** → **Merge pull request** → **Confirm merge**.
4. В GitKraken нажмите **Pull**, чтобы обновить локальную ветку `main`.
