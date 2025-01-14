use rustyline::{config, history::FileHistory};

pub struct LineReader {
    rl: rustyline::Editor<(), FileHistory>,
    history_file: String,
    prompt: String,
}

impl Drop for LineReader {
    fn drop(&mut self) {
        self.rl.save_history(&self.history_file).ok();
    }
}

pub enum LineReadStatus {
    Line(String),
    Done,
}

impl LineReader {
    pub fn new(history_file: &str, prompt: &str) -> LineReader {
        let config = rustyline::Config::builder()
            .history_ignore_space(true)
            .completion_type(config::CompletionType::List)
            .build();
        let mut rl = rustyline::Editor::with_config(config).unwrap();
        rl.load_history(&history_file).ok();
        LineReader {
            rl,
            history_file: history_file.into(),
            prompt: prompt.into(),
        }
    }

    pub fn readline(&mut self) -> LineReadStatus {
        let res = self.rl.readline(&self.prompt);

        match res {
            Ok(line) => {
                let _ = self.rl.add_history_entry(line.as_str());
                LineReadStatus::Line(line)
            }
            Err(_) => LineReadStatus::Done,
        }
    }
}
