#include <dirent.h>
#include <sys/stat.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <limits.h>

/* Helper to list directory and return entry names
   This avoids issues with dirent structure layout differences */
int list_dir_helper(const char *path, char names[][256], int max_entries) {
    DIR *dir;
    struct dirent *entry;
    int count = 0;

    dir = opendir(path);
    if (dir == NULL) {
        return 0;  /* Couldn't open directory */
    }

    while ((entry = readdir(dir)) != NULL && count < max_entries) {
        /* Skip . and .. */
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
            continue;
        }

        /* Copy name */
        strncpy(names[count], entry->d_name, 255);
        names[count][255] = '\0';  /* Ensure null termination */
        count++;
    }

    closedir(dir);
    return count;
}

/* Helper to check if path is a directory */
int is_dir_helper(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) {
        return 0;  /* stat failed */
    }
    return S_ISDIR(st.st_mode) ? 1 : 0;
}

/* Helper to check if path is a symlink */
int is_link_helper(const char *path) {
    struct stat st;
    if (lstat(path, &st) != 0) {
        return 0;  /* lstat failed */
    }
    return S_ISLNK(st.st_mode) ? 1 : 0;
}

/* Helper to get file size */
long long get_size_helper(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) {
        return 0;  /* stat failed */
    }
    return (long long)st.st_size;
}

/* Helper to check if running as root */
int is_running_as_root() {
    return (geteuid() == 0) ? 1 : 0;
}

/* Helper to get absolute path from relative path */
int get_absolute_path(const char *path, char *result, int result_len) {
    char *abs_path = realpath(path, NULL);
    if (abs_path == NULL) {
        return 0;  /* Failed to resolve path */
    }

    /* Copy to result buffer */
    strncpy(result, abs_path, result_len - 1);
    result[result_len - 1] = '\0';

    free(abs_path);
    return 1;  /* Success */
}
